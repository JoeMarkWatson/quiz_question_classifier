**The script in this repository is a machine learning model that classifies quiz questions into two categories**

Programming language: R

R packages: RCurl, tidyverse, googledrive, haven, dplyr, tm, stringr, splus2R, corpus, reclin, randomForest, caTools

This script demonstrates a successful classification of quiz questions into two categories dependent on subject matter, with questions and their original labels derived from a publicly available dataset containing questions from the show, Jeopardy.

The classifier takes roughly 2 hours to run using a training set of 4504 questions, giving an accuracy of almost 90% percent when applied to a test set containing 1501 questions.
