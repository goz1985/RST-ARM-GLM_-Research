#__________________ Modelling the bank churn solution__________________________
library(RoughSets)
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
churn$Geography<-factor(churn$Geography,labels = c("France","Spain","Germany"))



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


#_________________________________________________________Rough-set Theory Feature detection_______________________________

###..................creating a decision table to be used for the roughset method...........................................

churn_shuffled <- churn[sample(nrow(churn)),]
churn_decision_table <- SF.asDecisionTable(churn_shuffled,decision.attr = 11, indx.nominal = c(11))

#.....................Discretize the elements in the data sets for both training and testing................................

churn.cut.values <- D.discretization.RST(churn_decision_table,type.method = "local.discernibility") ### used localbecause of mixed attributes
churn_discretized <- SF.applyDecTable(churn_decision_table,churn.cut.values)


#.......................Feature selection using Greedy Heuristic Method where it computes the approximate reduct............
churn_GH_reduct <- FS.greedy.heuristic.reduct.RST(churn_discretized,qualityF = X.entropy,epsilon = 0.0)
churn.Gh.Reduct <- SF.applyDecTable(churn_decision_table, churn_GH_reduct)

write.csv(churn.Gh.Reduct,"D:\\Phd Research\\Experiment_2\\churn_GH.csv", row.names = FALSE)


#................compare with the Quick reduct method.............................................................................
churn_QR_reduct <- FS.quickreduct.RST(churn_discretized)
churn.QR.reduct<-SF.applyDecTable(churn_decision_table,churn_QR_reduct)

#' From the above two methods we see the attributes are all considered important by the rough set theory method of QR and Greedy heuristic
#' Next will be to use the discernibility matrix and see whether it will give us a better reduce data

churn.attr.p <- c(1,2,3,4,5,6,7,8,9,10,11)
churn.ind <- BC.IND.relation.RST(churn_discretized,feature.set = churn.attr.p)# defining the indiscernibility relation
system.time(churn_roughset <- BC.LU.approximation.RST(churn_discretized,churn.ind)) # defining the roughset
churn.region.rst <- BC.positive.reg.RST(churn_decision_table,churn_roughset)
system.time(churn.disc.mat<-BC.discernibility.mat.RST(churn_discretized,range.object = NULL))
bank.churn.reducts<-FS.all.reducts.computation(churn.disc.mat)
bank_churn_reduct<-SF.applyDecTable(churn_discretized,bank.churn.reducts)

#__________________________Association Rule Mining we are applying on the greedy heuristic__________________________________________

library(arules)
library(tidyverse)
library(arulesViz)
library(knitr)
library(gridExtra)
library(lubridate)

Churn_data.frame <- as.data.frame(churn.Gh.Reduct)
churn.transaction<-as(Churn_data.frame,"transactions")

itemLabels(churn.transaction)
image(churn.transaction)
itemFrequencyPlot(churn.transaction,topN=20,type='absolute')

##.................Mining the rules using different confidence and support measures.......................................,..

bank.rules <- apriori(churn.transaction,parameter = list(minlen = 2,supp= 0.01, conf = 0.8, maxlen=10),appearance = list(rhs= c("Exited=1", "Exited=0")))
summary(bank.rules)
rules_bank_df <- data.frame(lhs=labels(lhs(bank.rules)),rhs=labels(rhs(bank.rules)),bank.rules@quality)


bank.rules.sorted<- sort(bank.rules, by = "confidence", decreasing = TRUE)

## Prunning the rules of redundant or repeated rules#####

bank.subset.rules<-is.subset(bank.rules.sorted)
bank.subset.rules

bank.subset.rules[lower.tri(bank.subset.rules, diag=T)] <- F
bank.subset.rules

redundant<- apply(bank.subset.rules,2,any)
redundant

bank.rules.pruned <- bank.rules.sorted[!redundant]
arules::inspect(bank.rules.pruned)

bank.rules.df <- data.frame(lhs=labels(lhs(bank.rules.pruned)),rhs=labels(rhs(bank.rules.pruned)),bank.rules.pruned@quality)
