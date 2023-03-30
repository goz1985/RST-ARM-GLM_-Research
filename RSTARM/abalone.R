#.......................Experiment on the abalone dataset......................
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
abalone_df<-read.csv("D:/Datasets/Datasets for testing on model/abalone.csv")
str(abalone_df)
summary(abalone_df)

class(abalone_df$Sex)
abalone_df$Sex<-as.factor(abalone_df$Sex)
levels(abalone_df$Sex)

#.................Modelling on LM function.....................................
library(dplyr)
library(Metrics)
library(caret)
library(lmtest)

set.seed(1)
abalone_partition<- createDataPartition(abalone_df$Sex, p =0.75, list = FALSE, times = 1)
abalone_train <- abalone_df[abalone_partition,]
abalone_test <- abalone_df[-abalone_partition,]


#.........................Linear regression model the predictor variable has to be numeric not categorical..............................
glm_abalone<-glm(Length ~., data = abalone_train,family = gaussian,maxit=100)
summary(glm_abalone)
anova(glm_abalone,test = 'Chisq')
abalone_predict<-predict(glm_abalone,abalone_test,type="terms")
abalone_cut_off<-ifelse(abalone_predict>=0.5,1,0)
abalone_confusion_matrix<-confusionMatrix(as.factor(abalone_test$Length),as.factor(abalone_cut_off),positive = '1')
abalone_confusion_matrix
AIC(glm_abalone)
BIC(glm_abalone)



RMSE_ABALONE<-function(glm_abalone)
{
  dev = glm_abalone$deviance
  nullDev = glm_abalone$null.deviance
  modelN = length(glm_abalone$fitted.values)
  R.1 <- 1 - dev / nullDev
  R.cs <- 1- exp(-(nullDev-dev)/modelN)
  R.n <- R.cs /(1-(exp(-(nullDev / modelN))))
  cat("Pseudo R-sq for linear regression\n")
  cat("Hosmer & Lemeshow R-sq ", round(R.1, 4),"\n")
  cat("Cox and Snell R-sq ", round(R.cs, 4), "\n")
  cat("Nagelkerke R-sq ", round(R.n, 4), "\n")
}

RMSE_ABALONE(glm_abalone)

#_________________________________________________________Rough-set Theory Feature detection_______________________________

#" I have to look at this aspect of generating the features before feeding them 
###..................creating a decision table to be used for the rough-set method...........................................

abalone_shuffled <- abalone_df[sample(nrow(abalone_df)),]
abalone_decision_table <- SF.asDecisionTable(abalone_shuffled,decision.attr = 1, indx.nominal = c(1))

#.....................Discretize the elements in the data sets for both training and testing................................

abalone.cut.values <- D.discretization.RST(abalone_decision_table,type.method = "global.discernibility") 
abalone_discretized <- SF.applyDecTable(abalone_decision_table,abalone.cut.values)

#...............Using greedy heuristci to reduce the attributes...............................................
abalone_GH_reduct <- FS.greedy.heuristic.reduct.RST(abalone_discretized,qualityF = X.entropy,epsilon = 0.0)
abalone.Gh.Reduct <- SF.applyDecTable(abalone_decision_table, abalone_GH_reduct)
str(abalone.Gh.Reduct)
write.csv(abalone.Gh.Reduct,"D:\\Phd Research\\Experiment_2\\abalone_GH.csv", row.names = FALSE)


#...............Association rule mining process.............................................................
abalone_dataframe <- as.data.frame(abalone.Gh.Reduct) # Coerce Decision table back to data frame before coercing them to transactions
abalone_trans_GH<-as(abalone_dataframe,"transactions")
itemLabels(abalone_trans_GH)

abalone.rules <- apriori(abalone_trans_GH,parameter = list(minlen = 1,supp= 0.1, conf = 0.8, maxlen=999),appearance = list(rhs= c("Sex=F", "Sex=I","Sex=M")))

abalone.rules.sorted<- sort(abalone.rules, by = "confidence", decreasing = TRUE)

abalone.rules.df <- data.frame(lhs=labels(lhs(abalone.rules.sorted)),rhs=labels(rhs(abalone.rules.sorted)),abalone.rules.sorted@quality)
