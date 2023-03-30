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
bank_marketing_df<-read.csv("D:/Datasets/Datasets for testing on model/bank_marketing.csv")
str(bank_marketing_df)
bank_marketing_df$deposit<-factor(bank_marketing_df$deposit, labels = c("Yes","No"))


set.seed(1)
trainIndex <- createDataPartition(bank_marketing_df$deposit,
                                  p = 0.8, # training contains 80% of data
                                  list = FALSE)
dfTrain <- bank_marketing_df[ trainIndex,]
dfTest  <-bank_marketing_df[-trainIndex,]

control <- trainControl(method = "cv",
                        number = 10,
                        classProbs = TRUE,
                        summaryFunction = multiClassSummary)
#.................GLM model.......................................................
model_glm <- train(deposit~.,
                   data = dfTrain,
                   method = "glm",
                   family = "binomial",
                   # preProcess = "pca",
                   trControl = control)

print(model_glm)
summary(model_glm)

pred_glm_raw <- predict.train(model_glm,
                              newdata = dfTest,
                              type = "raw")

pred_glm_prob <- predict.train(model_glm,
                               newdata = dfTest,
                               type = "prob") 
confusionMatrix(data = pred_glm_raw,
                factor(dfTest$deposit),
                positive = "yes")


#.......................GLM model2...............................................
bank_glm <- glm(deposit~., data = dfTrain,family = "binomial")
summary(bank_glm)
anova(bank_glm)
#......................GAM......................................................
bank_gam<-gam(deposit ~., data = dfTrain,family = "binomial")
summary(bank_gam)


#.............Decision tree.......................................................
bank_tree <-rpart(deposit~.,data = dfTrain)
summary(bank_tree)
bank_tree_plot <-rpart.plot(bank_tree)

#_________________________________________________________Rough-set Theory Feature detection_______________________________

###..................creating a decision table to be used for the roughset method...........................................

bank_mkting_shuffled<- bank_marketing_df[sample(nrow(bank_marketing_df)),]
bank_decision_table <- SF.asDecisionTable(bank_mkting_shuffled,decision.attr = 17, indx.nominal = c(17))

#.....................Discretize the elements in the data sets for both training and testing................................

mkting.cut.values <- D.discretization.RST(bank_decision_table,type.method = "local.discernibility") ### used localbecause of mixed attributes
mkting_discretized <- SF.applyDecTable(bank_decision_table,mkting.cut.values)


#.......................Feature selection using Greedy Heuristic Method where it computes the approximate reduct............
mkting_RST<- FS.greedy.heuristic.reduct.RST(mkting_discretized,qualityF = X.entropy,epsilon = 0.1)
bank_mkting_GH.reduct <- SF.applyDecTable(bank_decision_table, mkting_RST)
str(bank_mkting_GH.reduct)

#....................Association rule mining............................................................................
bank.mkting.dataframe<- as.data.frame(bank_mkting_GH.reduct)
bank.mkting.transactions<-as(bank.mkting.dataframe,"transactions")
itemLabels(bank.mkting.transactions)

#..........Rule generation...........................................................................
mkting.rules <- apriori(bank.mkting.transactions,parameter = list(minlen = 2,supp= 0.01, conf = 0.8, maxlen=999),appearance = list(rhs= c("deposit=yes", "deposit=no")))

mkting.rules.sorted<- sort(mkting.rules, by = "confidence", decreasing = TRUE)

#........Prunning the rules of redundant or repeated rules...........................

mkting.subset.rules<-is.subset(mkting.rules.sorted)
mkting.subset.rules

mkting.subset.rules[lower.tri(mkting.subset.rules, diag=T)] <- F
mkting.subset.rules

redundant<- apply(mkting.subset.rules,2,any)
redundant

mkting.rules.pruned <- mkting.rules.sorted[!redundant]
arules::inspect(mkting.rules.pruned)

mkting.rules.df <- data.frame(lhs=labels(lhs(mkting.rules.pruned)),rhs=labels(rhs(mkting.rules.pruned)),mkting.rules.pruned@quality)

#....................Analyzing the binary values.............................................................
bank_binary_values<- read.csv("D:/Phd Research/Experiment_2/mkting_binary_values.csv")
str(bank_binary_values)
bank_binary_values$deposit<-factor(bank_binary_values$deposit, labels = c("Yes","No"))
bank_binary_values$Rule.instance<-NULL

bank.binary.glm<-glm(deposit~.,data = bank_binary_values,family = "binomial",maxit = 100)
print(bank.binary.glm)
anova(bank.binary.glm,test = 'Chisq')
AIC(bank.binary.glm)
BIC(bank.binary.glm)

#..................Compare model performance between the models.....................................
print(compare_performance(bank.binary.glm,bank_glm,bank_gam))
