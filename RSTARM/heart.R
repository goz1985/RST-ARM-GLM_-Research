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
heart_df<-read.csv("D:/Datasets/Datasets for testing on model/heart.csv")
str(heart_df)
summary(heart_df)

#...changing some of the attributes to factor...................................
heart_df$sex <- as.factor(heart_df$sex)
heart_df$cp <- as.factor(heart_df$cp)
heart_df$restecg <- as.factor(heart_df$restecg)
heart_df$exang <- as.factor(heart_df$exang)
heart_df$thal <- as.factor(heart_df$thal)
heart_df$target <- as.factor(heart_df$target)

heart_df$fbs<-as.factor(heart_df$fbs)
heart_df$slope<-as.factor(heart_df$slope)
heart_df$ca<-as.factor(heart_df$ca)
#..............Analyzing the dataset using models...............................
library(dplyr)
library(Metrics)
library(caret)
library(caretEnsemble)

set.seed(1)
heart_partition<- createDataPartition(heart_df$target, p =0.75, list = FALSE, times = 1)
heart_train <- heart_df[heart_partition,]
heart_test <- heart_df[-heart_partition,]

heart_ctrl <- trainControl(method="repeatedcv", number=10, repeats=20,summaryFunction = twoClassSummary,
                              savePredictions=TRUE, classProbs=TRUE,preProc = c("center","scale"))


glm_heart<-glm(target ~., data = heart_train,family = binomial,maxit=100)
print(glm_heart)
summary(glm_heart)
anova(glm_heart,test = 'Chisq')
hearts_predict<-predict(glm_heart,heart_test,type="response")
heart_cut_off<-ifelse(hearts_predict>=0.5,1,0)
heart_confusion_matrix<-confusionMatrix(as.factor(heart_test$target),as.factor(heart_cut_off),positive = '1')
heart_confusion_matrix
AIC(glm_heart)
BIC(glm_heart)



#............Modelling decision tree............................................
library(rpart)
library(rpart.plot)
heart_tree <-rpart(target ~., data = heart_train)
summary(heart_tree)
heart_tree_plot <-rpart.plot(heart_tree)
heart_tree_predict<-predict(heart_tree,heart_test,type='class')
confusionMatrix(heart_tree_predict,heart_test$target,positive = '1')

#.......................Modelling GAM...........................................
library(gam)
heart_gam<-gam(target ~., data = heart_train,family = binomial)
summary(heart_gam)
AIC(heart_gam)
BIC(heart_gam)
plot(heart_gam)

anova(glm_heart,heart_gam,test="Chisq")




#_________________________________________________________Rough-set Theory Feature detection_______________________________
#' check on this as it is generating an error while generating the reduct while using the greedy heuristic but 
#' Generates while using the Quick Reduct method.
###..................creating a decision table to be used for the rough-set method...........................................

heart_shuffled <- heart_df[sample(nrow(heart_df)),]
heart_decision_table <- SF.asDecisionTable(heart_shuffled,decision.attr = 14, indx.nominal = c(2,3,6,7,9,11,12,13,14))

#.....................Discretize the elements in the data sets for both training and testing................................

heart.cut.values <- D.discretization.RST(heart_decision_table,type.method = "local.discernibility") 
heart_discretized <- SF.applyDecTable(heart_decision_table,heart.cut.values)

#...............Using greedy heuristci to reduce the attributes...............................................
heart_GH_reduct <- FS.greedy.heuristic.reduct.RST(heart_discretized,qualityF = X.entropy,epsilon = 0.0)
heart.Gh.Reduct <- SF.applyDecTable(heart_decision_table, heart_GH_reduct)
str(heart.Gh.Reduct)

#...............Quick Reduct Method.........................................................................
heart_QR_reduct <- FS.quickreduct.RST(heart_discretized)
heart.QR.reduct<-SF.applyDecTable(heart_decision_table,heart_QR_reduct)

#...............Association rule mining process.............................................................
heart_dataframe <- as.data.frame(heart.Gh.Reduct) # Coerce Decision table back to data frame before coercing them to transactions
heart_trans_GH<-as(heart_dataframe,"transactions")
itemLabels(heart_trans_GH)

heart.rules <- apriori(heart_trans_GH,parameter = list(minlen = 4,supp= 0.01, conf = 0.8, maxlen=10),appearance = list(rhs= c("target=1", "target=0")))

heart.rules.sorted<- sort(heart.rules, by = "confidence", decreasing = TRUE)

## Prunning the rules of redundant or repeated rules#####

heart.subset.rules<-is.subset(heart.rules.sorted)
heart.subset.rules

heart.subset.rules[lower.tri(heart.subset.rules, diag=T)] <- F
heart.subset.rules

redundant.heart<- apply(heart.subset.rules,2,any)
redundant.heart

heart.rules.pruned <- heart.rules.sorted[!redundant.heart]
arules::inspect(heart.rules.pruned)

heart.rules.df <- data.frame(lhs=labels(lhs(heart.rules.pruned)),rhs=labels(rhs(heart.rules.pruned)),heart.rules.pruned@quality)

#..................Analyzing the binary values for the heart disease........................................

heart_binary <- read.csv("D:/Phd Research/Experiment_2/heart_binary_values.csv")
str(heart_binary)
heart_binary$target<-factor(heart_binary$target, labels = c("No","Yes"))
heart_binary$rule.No<-NULL

#......................Fitting a GLM model on the heart disease.............................................
heart.binary.glm<-glm(target~.,data = heart_binary,family = "binomial",maxit = 100)
print(heart.binary.glm)
anova(heart.binary.glm,test = 'Chisq')

print(compare_performance(heart.binary.glm,glm_heart,heart_gam))



