#.......................Experiment on the diabetes dataset......................
library(RoughSets)
library(RoughSetKnowledgeReduction)
library(googlesheets4)
library(arules)
library(dplyr)
library(RColorBrewer)
library(arulesViz)
library(tidyverse)
library(tidymodels)
library(ranger)
library(randomForest)
library(skimr)
library(ggplot2)
library(pROC)
library(plyr)
library(purrr)
library(car)

#-----Loading the dataset-------------------------------------------------------
horse_df<-read.csv("D:/Datasets/Datasets for testing on model/horse.csv")
str(horse_df)

horse_df$surgery<-factor(horse_df$surgery,labels = c("no","yes"))
horse_df$outcome<-factor(horse_df$outcome,labels = c("died","euthanized","lived"))

library(caret)
set.seed(1)
train_ind <- createDataPartition(horse_df$outcome, p= .8, list=FALSE)
training_set<-horse_df[train_ind,]
testing_set<-horse_df[-train_ind,]

#............GLM Model.........................................................
library(nnet)