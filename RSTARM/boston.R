#__________________ Modelling the bank churn solution__________________________
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
library(caret)
library(tidyverse)
library(MASS)

#-----Loading the dataset-------------------------------------------------------
#...stick to classification problems..........................
boston_df<-read.csv("D:/Datasets/Datasets for testing on model/boston.csv")
summary(boston_df)
str(boston_df)

#.............Model Building....................................................
library(dplyr)
library(Metrics)

set.seed(1)
boston_partition<- createDataPartition(boston_df$age, p =0.75, list = FALSE, times = 1)
boston_train <- boston_df[boston_partition,]
boston_test <- boston_df[-boston_partition,]

#.................Linear Regression model.......................................
boston_lm <- glm(age ~., data = boston_train,family = gaussian,maxit=100)
summary(boston_lm)
anova(boston_lm,test = 'Chisq')
#................Decision tree.................................................
library(rpart)
library(rpart.plot)
boston_tree <-rpart(age~.,data = boston_train)
boston_tree_plot <-rpart.plot(boston_tree)

summary(boston_tree)


#......................Roughset Theory reduct formation................................
###..................creating a decision table to be used for the roughset method...........................................

boston_shuffled <- boston_df[sample(nrow(boston_df)),]
boston_decision_table <- SF.asDecisionTable(boston_shuffled,decision.attr = 15, indx.nominal = c(15))

#.....................Discretize the elements in the data sets for both training and testing................................

boston.cut.values <- D.discretization.RST(boston_decision_table,type.method = "global.discernibility") ### used localbecause of mixed attributes
boston_discretized <- SF.applyDecTable(boston_decision_table,boston.cut.values)


#.......................Feature selection using Greedy Heuristic Method where it computes the approximate reduct............
boston_GH_reduct <- FS.greedy.heuristic.reduct.RST(boston_discretized,qualityF = X.entropy,epsilon = 0.0)
boston.Gh.Reduct <- SF.applyDecTable(boston_decision_table, boston_GH_reduct)
str(boston.Gh.Reduct)

#.................Association rule mining.................................................................
boston_data.frame <- as.data.frame(boston.Gh.Reduct)
boston.transaction<-as(boston_data.frame,"transactions")
itemLabels(boston.transaction)
