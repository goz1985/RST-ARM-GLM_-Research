##### In this new script I will use my proposed model on the breast cancer dataset####
## Loading the dataset and the perform EDA on the dataset####
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

cancer<-read.csv("D:/Datasets/Datasets for testing on model/breast-cancer.csv")
summary(cancer)
cancer %>% count(diagnosis)
cancer$diagnosis <- as.factor(cancer$diagnosis)
str(cancer)

skim_without_charts(cancer)
####Dividning into training and testing##########
library(dplyr)
library(Metrics)
library(caret)
library(caretEnsemble)

set.seed(1)
cancer_partition<- createDataPartition(cancer$diagnosis, p =0.75, list = FALSE, times = 1)
train <- cancer[cancer_partition,]
test <- cancer[-cancer_partition,]

cancer_ctrl <- trainControl(method="repeatedcv", number=10, repeats=20,summaryFunction = twoClassSummary,
                        savePredictions=TRUE, classProbs=TRUE,preProc = c("center","scale"))

#_______________________________Fitting a GLM model on the data________________________________________________

model_cancer <- train(diagnosis ~., data = train, method = 'glm',preProcess = c('center', 'scale'),trControl = cancer_ctrl,metric = 'ROC')
getTrainPerf(model_cancer)

##### making predictions with the trained model####
cancer_predict<-predict(model_cancer,test)
confusionMatrix(cancer_predict,test$diagnosis)

#________________________________Fitting a random forest model with the same data_____________________________________
randomForest_cancer <-train (diagnosis ~ .,data = train,method = 'rf',metric = 'ROC',tuneLength  = 15,trControl = cancer_ctrl)

system.time(fit.rf <- train(diagnosis~., data=train, method="rf",ntree= 100, metric='ROC', trControl=cancer_ctrl))
rf_predict <-predict(fit.rf,test)
confusionMatrix(rf_predict,test$diagnosis)
print(fit.rf)
plot(fit.rf)

#___________________________________Fitting GBM method__________________________________________________________
cancer_GBM<-train(diagnosis~.,data=train,method="gbm", trControl=cancer_ctrl,metric = 'ROC',tuneLength=3,verbose=FALSE)
GBM_predict<-predict(cancer_GBM,test)
GBM_frame<-data.frame(Orig=test,Pred=GBM_predict)
confusionMatrix(table(GBM_frame$Orig.diagnosis,GBM_frame$Pred))

#_____________________________________Fitting a decision tree model_______________________________________________
library(rattle)
Tree_cancer <- train(diagnosis~.,data=train,method="rpart", trControl=cancer_ctrl,metric = 'ROC')
fancyRpartPlot(Tree_cancer$finalModel)
Tree_predict<-predict(Tree_cancer,test)
Tree_frame<-data.frame(Orig=test,Pred=Tree_predict)
confusionMatrix(table(Tree_frame$Orig.diagnosis,Tree_frame$Pred))

#_________________________________________Rulefit Method original PRE________________________________________________________
library(pre)
system.time(cancer_pre<-pre(diagnosis~.,data = train,family = "binomial")) # Method takes abit of time to execute
cancer_cvpre <-cvpre(cancer_pre) ## assessing prediction error of the fitted PRE through k-fold of 10
cancer_cvpre$accuracy
#' for the above formula $SEL=0.031398628 which is the mean squared error on predicted values
#' $AEL = 0.076432577 which is the mean absolute error
#' $MCR = 0.04683841 which is the mis-classification error
#' $table shows the mis-classification errors for the obserations B and M.


coef_cancer <- coef(cancer_pre)
coef_cancer[1:6,]
cancer_pre_predict <- predict(cancer_pre,newdata= test,)
#____cancer_pre_frame <- data.frame(Orig=test,pred=cancer_pre_predict) # Check it later not working
#____confusionMatrix(table(cancer_pre_frame$Orig.diagnosis,cancer_pre_frame$pred)) # check it later




#__________________________________________ROUGHSET THEORY AND ARM ON THIS DATASET___________________________________________________________

cancer_shuffled <- cancer[sample(nrow(cancer)),]
cancer_shuffled$id<-NULL
cancer_DT <- SF.asDecisionTable(cancer_shuffled,decision.attr = 1, indx.nominal = c(1))


#___________Discretizing the dataset so as to use the Greedy heuristic method_________________

cancer_cutValues <- D.discretization.RST(cancer_DT,type.method = "global.discernibility")  
cancer_DT_discretized <- SF.applyDecTable(cancer_DT,cancer_cutValues)

#------------------FEATURE SELECTION USING THE GREEDY HEURISTIC MODEL------------------------------
Cancer_RST <- FS.greedy.heuristic.reduct.RST(cancer_DT_discretized,qualityF = X.entropy,epsilon = 0.1)
Cancer_Reduct_GH <- SF.applyDecTable(cancer_DT, Cancer_RST)
## Giving me only a reduct with two variables########################


#__________________Feature selection using Quick reduct____________________________________________

Cancer_QR_RST <- FS.feature.subset.computation(cancer_DT_discretized,method="quickreduct.rst")
Cancer_QR_Table<- SF.applyDecTable(cancer_DT, Cancer_QR_RST)

#______________________________________ Using the longer way____________________________________________
system.time(Cancer_IND <- BC.IND.relation.RST(cancer_DT_discretized))

rst_cancer <- BC.LU.approximation.RST(cancer_DT_discretized,Cancer_IND)
print(rst_cancer)

system.time(cancer.regions <- BC.positive.reg.RST(cancer_DT_discretized,rst_cancer))

system.time(cancer.discernibility.matrix <- BC.discernibility.mat.RST(cancer_DT_discretized,return.matrix = TRUE))
system.time(Cancer_reduct_all <- FS.all.reducts.computation(cancer.discernibility.matrix))


