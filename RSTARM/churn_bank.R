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

#-----Loading the dataset-------------------------------------------------------
churn<-read.csv("D:/Datasets/Datasets for testing on model/Churn_Modelling.csv")
str(churn)
churn$RowNumber <- NULL
churn$CustomerId  <- NULL
churn$Surname  <- NULL

#________________Convert numerical to factor for Geography,Gender,IsActiveMember,Exited, HasCrCard.

churn$Gender<-factor(churn$Gender, labels = c("0","1"))
churn$HasCrCard<-factor(churn$HasCrCard)
churn$IsActiveMember<-factor(churn$IsActiveMember)
churn$Exited<-factor(churn$Exited)
churn$Geography<-factor(churn$Geography,labels = c("0","1","2"))



str(churn)
#------------Exploratory data analysis------------------------------------------

library(inspectdf)

#--------------------------Applying The models for Prediction-----------------------------------------------

library(dplyr)
library(Metrics)
library(caret)
library(caretEnsemble)

set.seed(1)
churn_partition<- createDataPartition(churn$Exited, p =0.75, list = FALSE, times = 1)
churn_train <- churn[churn_partition,]
churn_test <- churn[-churn_partition,]

churn_ctrl <- trainControl(method="repeatedcv", number=10, repeats=20,summaryFunction = twoClassSummary,
                            savePredictions=TRUE, classProbs=TRUE,preProc = c("center","scale"))

#________________1. Logistic Regression_______________________________________________________________________________________________________________________________________

model_churn<-glm(Exited ~., data = churn_train,family = binomial)
summary(model_churn)
anova(model_churn,test = 'Chisq')
churn_predict<-predict(model_churn,churn_test,type="response")
churn_cut_off<-ifelse(churn_predict>=0.5,1,0)
churn_confusion_matrix<-confusionMatrix(as.factor(churn_test$Exited),as.factor(churn_cut_off),positive = '1')
churn_confusion_matrix
AIC(model_churn)
BIC(model_churn)
#_________________2. Rulefit method__________________________________________________________________________________________________________________________________________
library(pre)
system.time(churn_pre<-pre(Exited ~.,data = churn_train,family = "binomial")) # Method takes abit of time to execute
system.time(churn_cvpre <-cvpre(churn_pre)) ## assessing prediction error of the fitted PRE through k-fold of 10
churn_cvpre$accuracy
#' for the above formula $SEL=0.031398628 which is the mean squared error on predicted values
#' $AEL = 0.076432577 which is the mean absolute error
#' $MCR = 0.04683841 which is the mis-classification error
#' $table shows the mis-classification errors for the obserations B and M.
interact(churn_cvpre)
#' With the interact functionality we are able to check the interactions between the variables and how they behave
#' However the more the data, the more time it takes to compute these interactions.


#_________________________________________________GAM___________________________________________________________________
# Add the binomial link when dealing with classification aspects
library(gam)
churn_gam<-gam(Exited~.,data = churn_train,family = binomial)
summary(churn_gam)
AIC(churn_gam)
BIC(churn_gam)
plot(churn_gam)

anova(churn_gam,model_churn,test="Chisq")
churn_gam_predict <-predict(churn_gam,churn_test,type="response")

churn_gam_cutoff<-ifelse(churn_gam_predict>=0.5,1,0)
churn_gam_confusion_matrix<-confusionMatrix(as.factor(churn_test$Exited),as.factor(churn_gam_cutoff),positive = '1')
churn_gam_confusion_matrix

#### GLM and GAM for Logit are all but the same when it comes to their predictive responses

#_____________________________________________________DecisionTree_______________________________________________________

library(rpart)
library(rpart.plot)
churn_tree <-rpart(Exited~.,data = churn_train)
summary(churn_tree)
churn_tree_plot <-rpart.plot(churn_tree)
churn_tree_predict<-predict(churn_tree,churn_test,type='class')
confusionMatrix(churn_tree_predict,churn_test$Exited,positive = '1')


#_________________________________________________________Roughset Theory Feature detection_______________________________

