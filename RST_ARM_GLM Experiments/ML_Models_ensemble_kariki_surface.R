library(tidyverse)
library(caret)
library(ggplot2)
library(knitr)
library(lattice)
library(grid)
library(gridExtra)
library(ROCR)
library(corrplot)
library(dplyr)
library(RoughSets)
library(caretEnsemble)
library(fastAdaboost)
library(googlesheets4)
library(MLmetrics)
library(party)
library(e1071)
####### kariki_farm <- read.csv("D:/K_farm.csv")################ Refused to execute due to coding issues in the excel file thus loading from google sheets

k_farm <- read_sheet("https://docs.google.com/spreadsheets/d/1y29ch-sv9UXSZUX9mRxqx6NleN6-XO3ifXDqkDzeOiE/edit#gid=0")

#Loading the datasets, preprocessing. The dataset has missing values, for now I just choose to reove all rows with missing values
#Next I should choose an imputation method to see whether I can impute the missing values.

(n<-nrow(k_farm)) # Checking number of rows in the data which is 1538
c(as.character(k_farm$Date[1]), as.character(k_farm$Date[n])) # the date range from 2/3/2018 to 20/5/2021
head(k_farm$Rain.Yes.No.)
k_farm$Rain <- factor(k_farm$Rain) # Rain Yes/No to factor
k_farm$Date <- as.Date(k_farm$Date, '%m/%d/%Y') # Date column to date
str(k_farm)# When looking at the high and low numeric pressure,in the data set I have stored in google sheets its in numeric, leave as such
#k_farm$High.Hpa. <- as.numeric(k_farm$High.Hpa.)
#k_farm$Low.Hpa. <- as.numeric(k_farm$Low.Hpa.)
str(k_farm)
view(k_farm)
(cols_withNa <- apply(k_farm, 2, function(x) sum(is.na(x))))# Getting missing value tabulation

######## Picking only complete values, I create another data frame kariki_farm2################I removed missing value rows in the dataset
kariki_farm2 <- k_farm[complete.cases(k_farm),]
str(kariki_farm2)
(cols_withNa <- apply(kariki_farm2, 2, function(x) sum(is.na(x))))
kariki_farm2$Date <- NULL # removing the date column
kariki_farm2$Windspeed_low <- NULL # removing windsped low as it has no effect on the prediction 
kariki_farm2$Rain <- factor(kariki_farm2$Rain, labels = c("No","Yes"))
view(kariki_farm2)

############################Now applying several machine learning models on the data to see what it will output
set.seed(123)
kariki_train <- createDataPartition(kariki_farm2$Rain, p =0.75, list = FALSE)
training <- kariki_farm2[kariki_train,]
testing <- kariki_farm2[-kariki_train,]

#Ensemble Models############### Tree Bag and RF (Random Forest)#### Bagging
control <- trainControl(method="repeatedcv", number=10, repeats=3,
                        savePredictions=TRUE, classProbs=TRUE,preProc = c("center","scale"))

###Tree-Bag##### Working in HP
seed <- 7
metric <- "Accuracy"
set.seed(seed)
system.time(fit.treebag <- train(Rain~., data=training, method="treebag", metric=metric, trControl=control))

predictions_treebag<-predict(object=fit.treebag ,testing, type="prob")
table(predictions_treebag)
confusionMatrix(predictions_treebag,testing$Rain)

############## Random Forest model (Bagging model) ####################
set.seed(seed)
system.time(fit.rf <- train(Rain~., data=training, method="rf", metric=metric, trControl=control))

predictions_rf<-predict(object=fit.rf ,testing, type="raw")
table(predictions_rf)
confusionMatrix(predictions_rf,testing$Rain)


#########summarizing results of the two models##############################################################
############################################################################################################
bagging_results <- resamples(list(treebag=fit.treebag, rf=fit.rf))
summary(bagging_results)
dotplot(bagging_results)

#####Comparing the predictions using ROC curve..This part has an issue to show the ROC curve###############

pred_bag <- list(prediction_raw_Tbag,prediction_raw_RF)
pred_bag_predictions_treebag<-predict(object=fit.treebag ,testing, type="raw")
prediction_raw_Tbag <-  prediction(as.numeric(pred_bag_predictions_treebag), testing$Rain)

pred_bag_predictions_rf<-predict(object=fit.rf ,testing, type="raw")
prediction_raw_RF <-  prediction(as.numeric(pred_bag_predictions_rf), testing$Rain)

pred_bag <- data.frame(pred_bag)

caTools::colAUC(prediction_raw_Tbag,prediction_raw_RF, testing$Rain, plotROC = TRUE)
############################################################################################################
############################################################################################################

################### My Original method for boosting(gradient boosting)#######################################
system.time(kariki_gbm_fit_3 <- gbm(Rain ~ .,data = training,n.trees = 10000,interaction.depth = 1,shrinkage = 0.001,distribution = "gaussian",cv.folds = 5,n.cores = NULL, verbose = FALSE))
best.iter = gbm.perf(kariki_gbm_fit_3, method="cv")# Number of iterations for best output
plot.gbm(kariki_gbm_fit_3, 1, best.iter)

print(kariki_gbm_fit_3)
summary(kariki_gbm_fit_3)

predictions_gbm3<-predict(object=kariki_gbm_fit_3 ,testing, type="response")
table(predictions_gbm3)
confusionMatrix(kariki_gbm_fit_3)
# MSE and RMSE
sqrt(min(kariki_gbm_fit$cv.error))
sqrt(min(kariki_gbm_fit_3$cv.error)) # had an mse of 0.03

#Plotting the loss function
gbm.perf(kariki_gbm_fit, method = "cv")
gbm.perf(kariki_gbm_fit_3, method = "cv")

# Tuning the model even further by reducing the no of trees abd the depth
kariki_gbm_fit_2 <- gbm(Rain ~ .,data = training,n.trees = 5000,interaction.depth = 3,shrinkage = 0.1,distribution = "adaboost",cv.folds = 5,n.cores = NULL, verbose = FALSE)  
###############################################################################################################################################




##################From R Pubs############################################## Working for Surface
index = createDataPartition(y=kariki_farm2$Rain, p=0.7, list=FALSE)

train = kariki_farm2[index,]
test = kariki_farm2[-index,]
dim(train)

### Choosing the predictors for your model will yield different results..
#' First I had chosen high temperature and it yielded

train.predictors = data.frame(kariki_farm2 = train$Rain)
train.rain = train$Rain

test.predictors = data.frame(kariki_farm2 = test$Rain)
test.rain = test$Rain

treebag = bag(train.predictors, train.rain, B=100,
              bagControl = bagControl(fit = ctreeBag$fit,
                                      predict = ctreeBag$pred,
                                      aggregate = ctreeBag$aggregate),
              trControl = trainControl(method = "oob"))

summary(treebag)
ConfusionMatrix(treebag)
#### Giving me error not OPTIMAL FOR FACTORS..##########
####REQUIRES NUMERICAL VALUES, generating the confusion matrix###
rain.pred = predict(treebag, test.predictors,type="response")
MSE = mean(test.rain - rain.pred)^2

###Boosting models#####
library(gbm)

# With a a different distribution of bernouli or Gaussian( square error) because rain is factor
# For Bernoulli, it had an error using trees

set.seed(123)
fitControl = trainControl(method="cv", number=5, returnResamp = "all")
model2 = train(Rain~., data=training, method="gbm",distribution="bernoulli", trControl=fitControl, verbose=F, tuneGrid=data.frame(.n.trees=best.iter, .shrinkage=0.01, .interaction.depth=1, .n.minobsinnode=1))
confusionMatrix(model2)
mPred = predict(model2, testing, na.action = na.pass)
postResample(mPred, testing$Rain)
confusionMatrix(mPred, testing$Rain)
getTrainPerf(model2)

mResults = predict(model2, testing, na.action = na.pass, type = "prob")
mResults$obs = testing$Rain
head(mResults)

mnLogLoss(mResults, lev = levels(mResults$obs))

mResults$pred = predict(model2,testing, na.action = na.pass)
multiClassSummary(mResults, lev = levels(mResults$obs))### LOg loss in 0.02 way better than my original model


########## Employing the GLM model on the data
trControl <- trainControl(method = "repeatedcv",  repeats = 5, number = 10, verboseIter = FALSE)
predictor_rain <-c("High_Temp","Avg_Temp","Low_Temp","Dewpoint_High","Dewpoint_Avg","Dewpoint_low","Humidity_High","Humidity_Avg","Humidity_Low","Windspeed_High","Windspeed_Avg")
prediction_formula <- as.formula(paste("Rain", paste(predictor_rain, collapse="+"), sep="~"))
kariki_ML_models <- train(prediction_formula,data = training,method = "glm",family="binomial", trControl = trControl, metric = 'Accuracy',maxit = 100)
kariki_ML_models$results$Accuracy
summary(kariki_ML_models) # From the summary of the model


#########Using RoughSets First Experiment###############################################################################
## I used the discernibility matrix and the all reduct to get the reducts that can be generated from the dataset
kariki_shuffled <- kariki_farm2[sample(nrow(kariki_farm2)),]
# I set the indx.nominal between 1:15 and all the discetization methods but when set to between 1:13 it worked..
# Error message is "All the conditional attributes are already nominal."
kariki_DT <- SF.asDecisionTable(kariki_shuffled, decision.attr = 15, indx.nominal = c(1:14))

# Computing the indiscernibility relation
IND <- BC.IND.relation.RST(kariki_DT)

# Compute Upper and Lower Approximations
kariki_roughset <- BC.LU.approximation.RST(kariki_DT,IND)

# Determining the regions
kariki.regions <- BC.positive.reg.RST(kariki_DT,kariki_roughset)

# Discernibly matrix formulation
system.time(kariki.matrix <- BC.discernibility.mat.RST(kariki_DT))

### Computation of all possible reducts that can be gotten from the discernibility matrix#####
# Method generated 484 reducts in total and thus generated only 10 because I had 10 different reducts
system.time(reduct_all <- FS.all.reducts.computation(kariki.matrix))
dec_table_1 <- SF.applyDecTable(kariki_DT, reduct_all, control = list(indx.reduct = 1))
dec_table_2 <- SF.applyDecTable(kariki_DT, reduct_all, control = list(indx.reduct = 2))
dec_table_3 <- SF.applyDecTable(kariki_DT, reduct_all, control = list(indx.reduct = 3))
dec_table_4 <- SF.applyDecTable(kariki_DT, reduct_all, control = list(indx.reduct = 4))
dec_table_5 <- SF.applyDecTable(kariki_DT, reduct_all, control = list(indx.reduct = 5))
dec_table_6 <- SF.applyDecTable(kariki_DT, reduct_all, control = list(indx.reduct = 6))
dec_table_7 <- SF.applyDecTable(kariki_DT, reduct_all, control = list(indx.reduct = 7))
dec_table_8 <- SF.applyDecTable(kariki_DT, reduct_all, control = list(indx.reduct = 8))
dec_table_9 <- SF.applyDecTable(kariki_DT, reduct_all, control = list(indx.reduct = 9))
dec_table_10 <- SF.applyDecTable(kariki_DT, reduct_all, control = list(indx.reduct = 10))


######Comparing metrics when used with machine learning methods on Reduct 1, Reduct 4###########

#############Reduct 1###############################

index_1 = createDataPartition(y=dec_table_1$Rain, p=0.7, list=FALSE)

train_Reduct_1 = dec_table_1[index_1,]
test_Reduct_1 = dec_table_1[-index_1,]
dim(train_Reduct_1)

### Choosing the predictors for your model will yield different results..
#' First I had chosen high temperature and it yielded

train_Reduct_1.predictors = data.frame(dec_table_1 = train_Reduct_1$Rain)
train_Reduct_1.rain = train_Reduct_1$Rain

test_Reduct_1.predictors = data.frame(dec_table_1 = test_Reduct_1$Rain)
test_Reduct_1.rain = test_Reduct_1$Rain

treebag_R1 = bag(train_Reduct_1.predictors, train_Reduct_1.rain, B=100,
                          bagControl = bagControl(fit = ctreeBag$fit,
                                                  predict = ctreeBag$pred,
                                                  aggregate = ctreeBag$aggregate),
                          trControl = trainControl(method = "oob"))

summary(treebag_R1)
ConfusionMatrix(treebag_R1)

#############Reduct 4###############################

index_4 = createDataPartition(y=dec_table_4$Rain, p=0.7, list=FALSE)

train_Reduct_4 = dec_table_4[index_1,]
test_Reduct_4 = dec_table_4[-index_1,]
dim(train_Reduct_4)

### Choosing the predictors for your model will yield different results..
#' First I had chosen high temperature and it yielded

train_Reduct_4.predictors = data.frame(dec_table_4 = train_Reduct_4$Rain)
train_Reduct_4.rain = train_Reduct_4$Rain

test_Reduct_4.predictors = data.frame(dec_table_4 = test_Reduct_4$Rain)
test_Reduct_4.rain = test_Reduct_4$Rain

treebag_R4 = bag(train_Reduct_4.predictors, train_Reduct_1.rain, B=100,
                 bagControl = bagControl(fit = ctreeBag$fit,
                                         predict = ctreeBag$pred,
                                         aggregate = ctreeBag$aggregate),
                 trControl = trainControl(method = "oob"))

summary(treebag_R4)
ConfusionMatrix(treebag_R1)


####Using My earlier treebag model on the reduct from the roughset model##########
control <- trainControl(method="repeatedcv", number=10, repeats=3,
                        savePredictions=TRUE, classProbs=TRUE,preProc = c("center","scale"))

###Tree-Bag with reduct_1##### Working in HP
seed <- 7
metric <- "Accuracy"
set.seed(seed)
system.time(fit.treebag <- train(Rain~., data=train_Reduct_1, method="treebag", metric=metric, trControl=control))

predictions_treebag<-predict(object=fit.treebag ,test_Reduct_1, type="raw")
table(predictions_treebag)
confusionMatrix(predictions_treebag,test_Reduct_1$Rain)


################ Using Random Forest ########################
### Though not optimal...##########
system.time(fit.rf_R1 <- train(Rain~., data=train_Reduct_1, method="rf", metric=metric, trControl=control))
predictions_rf_R1<-predict(object=fit.rf_R1 ,test_Reduct_1, type="raw")
table(predictions_rf_R1)
confusionMatrix(predictions_rf_R1,test_Reduct_1$Rain)



####Trying the randomForest ex-plainer to model as a random forest model########
##### Realized that high_temp, Dew_point,Precipitation_amount were taken as factors instead of numeric###

library(randomForest)
train_Reduct_1$High_Temp = as.numeric(train_Reduct_1$High_Temp)
train_Reduct_1$Dewpoint_High = as.numeric(train_Reduct_1$Dewpoint_High)
train_Reduct_1$Precipitation_amount = as.numeric(train_Reduct_1$Precipitation_amount)
model <- randomForest(Rain~., data = train_Reduct_1,ntree =10000)
print(model)

oob.err.data <- data.frame(
  Trees = rep(1:nrow(model$err.rate), 3), 
  Type = rep(c("OOB","Yes","No"), each = nrow(model$err.rate)),
  Error = c(model$err.rate[,"OOB"], model$err.rate[,"Yes"], model$err.rate[,"No"]))

ggplot(data = oob.err.data, aes(x = Trees, y= Error)) + geom_line(aes(color = Type))
importance(model)
varImpPlot(model)

##################Boosting ensemble#################################################

# With a a different distribution of bernouli or Gaussian( square error) because rain is factor
# For Bernoulli, it had an error using trees

set.seed(123)
fitControl_1 = trainControl(method="cv", number=5, returnResamp = "all")
model_R1 = train(Rain~., data=train_Reduct_1, method="gbm",distribution="bernoulli", trControl=fitControl_1, verbose=F, tuneGrid=data.frame(.n.trees=best.iter, .shrinkage=0.01, .interaction.depth=1, .n.minobsinnode=1))
confusionMatrix(model_R1)
mPred_R1 = predict(model_R1, test_Reduct_1, na.action = na.pass)
postResample(mPred_R1, test_Reduct_1$Rain)
confusionMatrix(mPred_R1, test_Reduct_1$Rain)
getTrainPerf(model_R1)

mResults_R1 = predict(model_R1, test_Reduct_1, na.action = na.pass, type = "prob")
mResults_R1$obs = test_Reduct_1$Rain
head(mResults_R1)

mnLogLoss(mResults_R1, lev = levels(mResults_R1$obs))

mResults_R1$pred = predict(model_R1,test_Reduct_1, na.action = na.pass)
multiClassSummary(mResults_R1, lev = levels(mResults_R1$obs))### LOg loss in 0.02 way better than my original model





##### Approximations calculations######################
low_app_no <- kariki_roughset$lower.approximation$`0`
upp_app_no <- kariki_roughset$upper.approximation$`0`
low_app_yes <- kariki_roughset$lower.approximation$`1`
upp_app_yes <- kariki_roughset$upper.approximation$`1`

boundary_app_yes <- setdiff(upp_app_yes,low_app_yes)
boundary_app_no <- setdiff(upp_app_no,low_app_no)

Uni_discourse <- c(1:nrow(kariki_DT))
outer_region = setdiff(Uni_discourse,boundary_app_yes)

print(outer_region)
print(low_app_yes)
print(kariki_roughset)



#########Using Rough sets Second Experiment###############################################################################
kariki_shuffled <- kariki_farm2[sample(nrow(kariki_farm2)),]
# I set the indx.nominal between 1:15 and all the discetization methods but when set to between 1:13 it worked..
# Error message is "All the conditional attributes are already nominal."
kariki_DT <- SF.asDecisionTable(kariki_shuffled, decision.attr = 15, indx.nominal = c(1:14))
######Testing all discretization methods, all conditional attributes are nominal##################
kariki_DT_cutValues <- D.discretization.RST(kariki_DT, type.method = "unsupervised.quantiles")
##################################################################################################