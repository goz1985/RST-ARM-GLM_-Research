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
<<<<<<< HEAD
library(googlesheets4)

kariki_farm <- read.csv("D:/K_farm.csv")

k_farm <- read_sheet("https://docs.google.com/spreadsheets/d/1y29ch-sv9UXSZUX9mRxqx6NleN6-XO3ifXDqkDzeOiE/edit#gid=0")
=======
kariki_farm <- read.csv("E:/Datasets/Research datasets/Weather data/Kariki_Farm.csv")
>>>>>>> 5fde6696ade2ffe214ac4d06509120803ca9fec3

#Loading the datasets, preprocessing. The dataset has missing values, for now I just choose to reove all rows with missing values
#Next I should choose an imputation method to see whether I can impute the missing values.

(n<-nrow(k_farm)) # Checking number of rows in the data which is 1179
c(as.character(k_farm$Date[1]), as.character(k_farm$Date[n])) # the date range from 2/3/2018 to 20/5/2021
head(k_farm$Rain.Yes.No.)
k_farm$Rain <- factor(k_farm$Rain) # Rain Yes/No to factor
k_farm$Date <- as.Date(k_farm$Date, '%m/%d/%Y') # Date column to date
str(k_farm)# When looking at the high and low numeric pressure, I had to convert them to numeric because they were being considered as being factors yet theyr weren't
k_farm$High.Hpa. <- as.numeric(k_farm$High.Hpa.)
k_farm$Low.Hpa. <- as.numeric(k_farm$Low.Hpa.)
str(k_farm)
view(k_farm)
(cols_withNa <- apply(k_farm, 2, function(x) sum(is.na(x))))

######## Picking only complete values, I create another data frame kariki_farm2################I removed missing value rows in the dataset
kariki_farm2 <- k_farm[complete.cases(k_farm),]
str(kariki_farm2)
(cols_withNa <- apply(kariki_farm2, 2, function(x) sum(is.na(x))))# Checking if missing values have been removed
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

###Tree-Bag
seed <- 7
metric <- "Accuracy"
set.seed(seed)
system.time(fit.treebag <- train(Rain~., data=training, method="treebag", metric=metric, trControl=control))

predictions_treebag<-predict(object=fit.treebag ,testing, type="prob")
table(predictions_treebag)
confusionMatrix(predictions_treebag,testing$Rain)


#RF model
set.seed(seed)
mtry_def <- floor(sqrt(ncol(training))*.75)
t_grid <- expand.grid(mtry = c(mtry_def))
system.time(fit.rf <- train(Rain~., data=training, method="rf",ntree= 100, metric=metric, trControl=control,tuneGrid = t_grid))

predictions_rf<-predict(object=fit.rf ,testing, type="raw")

table(predictions_rf)
confusionMatrix(predictions_rf,testing$Rain)

print(fit.rf)# Summary for the random forest model, if I define the number of trees in the RF model, the accuracy goes down

### Printing RMSE####error with code line 75 check on how to get mse and rmse

predictions <- predict(fit.rf, testing, type = "raw")
RMSE <- sqrt(sum((predictions - testing)^2)/length(predictions))

#summarizing results of the two models

bagging_results <- resamples(list(treebag=fit.treebag, rf=fit.rf))
summary(bagging_results)
dotplot(bagging_results)

#Comparing the predictions using ROC curve..This part has an issue to show the ROC curve

pred_bag <- list(predictions_treebag,predictions_rf)
pred_bag_predictions_treebag<-predict(object=fit.treebag ,testing, type="raw")
prediction_raw_Tbag <-  prediction(as.numeric(pred_bag_predictions_treebag), testing$Rain)

pred_bag_predictions_rf<-predict(object=fit.rf ,testing, type="raw")
prediction_raw_RF <-  prediction(as.numeric(pred_bag_predictions_rf), testing$Rain)

pred_bag <- data.frame(pred_bag)

caTools::colAUC(prediction_raw_Tbag,prediction_raw_RF, testing$Rain, plotROC = TRUE)

### Can use the MLeval library to evaluate the models####
library(MLeval)
res <- evalm(list(fit.treebag,fit.rf),gnames = 'treebag','rf')



###Boosting models#####
library(gbm)
set.seed(123)
system.time(kariki_gbm_fit <- gbm(Rain ~ .,data = training,n.trees = 10000,interaction.depth = 1,shrinkage = 0.001,distribution = "adaboost",cv.folds = 5,n.cores = NULL, verbose = FALSE))

predictions_gbm<-predict(object=kariki_gbm_fit ,testing, type="link")

table(predictions_gbm)
confusionMatrix(predictions_gbm,testing$Rain)
# With a a different distribution of bernouli or gaussian( square error) because rain is factor
# For bernoulli, it had an error using trees
kariki_gbm_fit_3 <- gbm(Rain ~ .,data = training,n.trees = 10000,interaction.depth = 1,shrinkage = 0.001,distribution = "gaussian",cv.folds = 5,n.cores = NULL, verbose = FALSE)  
summary(kariki_gbm_fit_3)
print(kariki_gbm_fit_3)
# MSE and RMSE
sqrt(min(kariki_gbm_fit$cv.error))
sqrt(min(kariki_gbm_fit_3$cv.error)) # had an mse of 0.03

#Plotting the loss function
gbm.perf(kariki_gbm_fit, method = "cv")
gbm.perf(kariki_gbm_fit_3, method = "cv")

# Tuning the model even further by reducing the no of trees abd the depth
kariki_gbm_fit_2 <- gbm(Rain ~ .,data = training,n.trees = 5000,interaction.depth = 3,shrinkage = 0.1,distribution = "adaboost",cv.folds = 5,n.cores = NULL, verbose = FALSE)  
sqrt(min(kariki_gbm_fit_2$cv.error))


########## Employing the GLM model on the data##### Accuracy of 83%
predictor_rain <-c("High_Temp","Avg_Temp","Low_Temp","Dewpoint_High","Dewpoint_Avg","Dewpoint_low","Humidity_High","Humidity_Avg","Humidity_Low","Windspeed_High","Windspeed_Avg")
prediction_formula <- as.formula(paste("Rain", paste(predictor_rain, collapse="+"), sep="~"))
system.time(kariki_ML_models <- train(prediction_formula,data = training,method = "glm",family="binomial", trControl = control, metric = 'Accuracy',maxit = 100))
kariki_ML_models$results$Accuracy
summary(kariki_ML_models) # From the summary of the model

glm_responses <- predict(kariki_ML_models,testing,type = "raw")
table(glm_responses)
confusionMatrix(glm_responses,testing$Rain)
glm_responses

# Calculating the MSE and RMSE


#########Using Roughsets#########################
kariki_shuffled <- kariki_farm2[sample(nrow(kariki_farm2)),]
# I set the indx.nominal between 1:15 and all the discretization methods but when set to between 1:13 it worked..
# Error message is "All the conditional attributes are already nominal."
kariki_DT <- SF.asDecisionTable(kariki_shuffled, decision.attr = 16, indx.nominal = c(1:13))

kariki_DT_cutValues_2 <- D.discretization.RST(kariki_DT, type.method = "local.discernibility") # Method refused due to the date attribute hence used the unsupervised quantiles
kariki_DT_cutValues <- D.discretization.RST(kariki_DT, type.method = "global.discernibility")# Testing both methods

# Deducing the new discretized tables

kariki_Table_Discretized <- SF.applyDecTable(kariki_DT,kariki_DT_cutValues)
kariki_Table_Discretized_2 <- SF.applyDecTable(kariki_DT,kariki_DT_cutValues_2)

# Indiscernibility relations. Compute them from decision tables. WIll choose a different approach
# First compute a decision reduct...approximate reducts which contain a subset of features
Comp_reduct_1 <- FS.reduct.computation(kariki_Table_Discretized) # attributes are High_temp, Dew_point_Avg and Windspeed_Avg
Comp_reduct_2 <- FS.reduct.computation(kariki_Table_Discretized_2) # attributes are High_temp, Dewpoint_High,Precipitation_amount

Com

IND_RELATION_1 <- BC.IND.relation.RST(kariki_Table_Discretized,feature.set = Comp_reduct_1)
IND_RELATION_2 <- BC.IND.relation.RST(kariki_Table_Discretized,feature.set = Comp_reduct_2)

IND_R_1 <- BC.IND.relation.RST(kariki_DT,feature.set = Comp_reduct_1)
IND_R_2 <- BC.IND.relation.RST(kariki_DT,feature.set = Comp_reduct_2)# Issues not generating idiscernibility relation till the DT is discretized.

# Getting the roughset for the above indiscernibility relations

roughset_1 <- BC.LU.approximation.RST(kariki_Table_Discretized,IND_RELATION_1)
roughset_2 <- BC.LU.approximation.RST(kariki_Table_Discretized_2,IND_RELATION_2)

# THis aspect isnt returning the indiscernible objects very clearly..still more work to do here.

######Discernibility matrix computation
disc.mat<- BC.discernibility.mat.RST(kariki_DT, range.object = NULL)
disc.mat_1<- BC.discernibility.mat.RST(kariki_Table_Discretized, range.object = NULL)
disc.mat_2<- BC.discernibility.mat.RST(kariki_Table_Discretized_2, range.object = NULL)


#Reduct computation and formulation, Have to research more because am getting very many reducts upto 366

reduct <- FS.all.reducts.computation(disc.mat) #Time consuming..or either my machine resources are low
reduct_1 <- FS.all.reducts.computation(disc.mat_1)
reduct_2 <- FS.all.reducts.computation(disc.mat_2)

reduct_table_1 <- SF.applyDecTable(kariki_DT, reduct_1, control = list(indx.reduct = 1))

reduct_table_2 <- SF.applyDecTable(kariki_DT, reduct_2, control = list(indx.reduct = 30))