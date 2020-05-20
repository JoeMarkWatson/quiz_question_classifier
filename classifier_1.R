#ML model classifying quiz show questions into sport and history categories 

#model run time is approx 2 hours

#load packages
library(RCurl)
library(tidyverse) 
library(googledrive) 
library(haven)
library(dplyr)
library(tm)
library(stringr)
library(splus2R)
library(corpus)
library(reclin)
library(randomForest)
library(caTools)

#clear global environment
rm(list=ls())

#import df via cambridgespark blog: https://blog.cambridgespark.com/50-free-machine-learning-datasets-natural-language-processing-d88fb9c5c8da
JEOPARDY_CSV <- read.csv("https://drive.google.com/uc?export=download&id=0BwT5wj_P7BKXUl9tOUJWYzVvUjA")
nrow(table(JEOPARDY_CSV$Category)) #shows a large number of categories (of which many are sport- or history-based) #as such, all questions with labels predominantly to do with either sport or history are grouped together

#print all rows where cat. inc.s 'sport'
z<-JEOPARDY_CSV[grep("SPORT", JEOPARDY_CSV$Category), ]
z$Category<-as.character(z$Category)
as.data.frame(table(z$Category))
#remove any row where $Category holds string "TRANSPORT", "ILLUSTRATED" or "PSEUDO SPORT LIT"
non_rel <- c("TRANSPORT", "ILLUSTRATED", "PSEUDO SPORT LIT", "HISTORY")
sports<-z[!(grepl(paste(non_rel,collapse="|"), z$Category)), ]
sports$classification<-"sports"

#print all rows where cat. inc.s 'history'
z<-JEOPARDY_CSV[grep("HISTORY", JEOPARDY_CSV$Category), ]
z$Category<-as.character(z$Category)
as.data.frame(table(z$Category))
#remove any row where $Category holds string "SPORT" to remove overlap
non_rel <- c("SPORT")
history<-z[!(grepl(paste(non_rel,collapse="|"), z$Category)), ]
history$classification<-"history"

#rbind sport and history df.s
NLPset<-rbind(sports, history) 
#converting columns to appropriate format (bar Value, which is "None" for final jeopardy so remains a factor)
NLPset$Question<-as.character(NLPset$Question)
NLPset$Answer<-as.character(NLPset$Answer)
NLPset$classification_sport<-TRUE
NLPset[(NLPset$classification!="sports"),"classification_sport"]<-FALSE #giving a logical var, TRUE (if sports) or FALSE (if history)
NLPset$Air.Date<-as.Date(NLPset$Air.Date)
str(NLPset) #checking appropriate formats throughout

#clean the Question column
#gsubbing "u.s", "www.j" and "_blank" for ""
NLPset$Question<-gsub("www.j", "", NLPset$Question)
NLPset$Question<-gsub("_blank", "", NLPset$Question)
NLPset$Question<-gsub("archive.com", "", NLPset$Question)
NLPset$Question<-gsub("u.s", "America", NLPset$Question)
NLPset$Question <- str_replace_all(NLPset$Question, "[^[:alnum:]]", " ")
NLPset$Question <- lowerCase(NLPset$Question)
NLPset$Question <- removeWords(NLPset$Question, stopwords("en"))
new_stops <- c("href", "http", "media", "www j ", "target", "blank", "jpg")
NLPset$Question <- removeWords(NLPset$Question, new_stops)

#adding a doc_id column and renaming Question as text (with these columns req.d for the establishment of a VCorpus)
names(NLPset)[names(NLPset)=="Question"] <- "text"
NLPset$doc_id<-seq(length = nrow(NLPset))

#now establishing VCorpus itself
df_source <- DataframeSource(NLPset)
df_corpus <- VCorpus(df_source) #convert df_source to a corpus: df_corpus
df_corpus #examine df_corpus
meta(df_corpus) #examine df_corpus metadata
# processing and stem (although processing largely done previously)
df_corpus_pro <- tm_map(df_corpus, stripWhitespace)
df_corpus_pro <- tm_map(df_corpus_pro, stemDocument)

#represent text using bag of words
#DTM creation
question_dtm <- DocumentTermMatrix(df_corpus_pro)
question_m <- as.matrix(question_dtm)
less_sparse_matrix <- removeSparseTerms(question_dtm, sparse =.9996)
less_sparse_matrix #gives number of terms retained
data <- as.data.frame(as.matrix(less_sparse_matrix))
data$classication <- NLPset$classification_sport
data$classication <- as.character(data$classication) #row added following to address str issue, recognised in: https://discuss.analyticsvidhya.com/t/what-does-the-warning-the-response-has-five-or-fewer-unique-values-while-building-random-forest-mean/6442/2
data$classication <- as.factor(data$classication) #row added following to address str issue, recognised in: https://discuss.analyticsvidhya.com/t/what-does-the-warning-the-response-has-five-or-fewer-unique-values-while-building-random-forest-mean/6442/2
set.seed(1234)
splitz <- sample.split(data$classication, SplitRatio = 0.75) #using 'splitz' as 'split' was an existing var name
training_set <- subset(data, splitz == TRUE)
test_set <- subset(data, splitz == FALSE)
classifier <- randomForest(x = training_set[-3419], 
                           y = training_set$classication,
                           nTree = 10)
y_pred <- predict(classifier, newdata = test_set[-3419])
cm <- table(test_set[, 3419], y_pred)
cm #prints confusion matrix
accuracy<-(cm[1,1]+cm[2,2])/sum(cm) 
accuracy #prints model accuracy

