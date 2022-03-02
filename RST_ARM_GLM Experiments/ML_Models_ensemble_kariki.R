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
kariki_farm <- read.csv("E:/Datasets/Research datasets/Weather data/Kariki_Farm.csv")

#Loading the datasets, preprocessing. The dataset has missing values, for now I just choose to reove all rows with missing values
#Next I should choose an imputation method to see whether I can impute the missing values.

(n<-nrow(kariki_farm)) # Checking number of rows in the data which is 1179
c(as.character(kariki_farm$Date[1]), as.character(kariki_farm$Date[n])) # the date range from 2/3/2018 to 20/5/2021
head(kariki_farm$Rain.Yes.No.)
kariki_farm$Rain <- factor(kariki_farm$Rain) # Rain Yes/No to factor
kariki_farm$Date <- as.Date(kariki_farm$Date, '%m/%d/%Y') # Date column to date
str(kariki_farm)# When looking at the high and low numeric pressure, I had to convert them to numeric because they were being considered as being factors yet theyr weren't
kariki_farm$High.Hpa. <- as.numeric(kariki_farm$High.Hpa.)
kariki_farm$Low.Hpa. <- as.numeric(kariki_farm$Low.Hpa.)
str(kariki_farm)
view(kariki_farm)
(cols_withNa <- apply(kariki_farm, 2, function(x) sum(is.na(x))))

######## Picking only complete values, I create another data frame kariki_farm2################I removed missing value rows in the dataset
kariki_farm2 <- kariki_farm[complete.cases(kariki_farm),]
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

#TreeBag
seed <- 7
metric <- "Accuracy"
set.seed(seed)
fit.treebag <- train(Rain~., data=training, method="treebag", metric=metric, trControl=control)

predictions_treebag<-predict(object=fit.treebag ,testing, type="raw")
table(predictions_treebag)
confusionMatrix(predictions_treebag,testing$Rain)

#RF model
set.seed(seed)
fit.rf <- train(Rain~., data=training, method="rf", metric=metric, trControl=control)

predictions_rf<-predict(object=fit.rf ,testing, type="raw")
table(predictions_rf)
confusionMatrix(predictions_rf,testing$Rain)

#summarizing results of the two models

bagging_results <- resamples(list(treebag=fit.treebag, rf=fit.rf))
summary(bagging_results)
dotplot(bagging_results)

#Comparing the predictions using ROC curve..This part has an issue to show the ROC curve

pred_bag <- list(prediction_raw_Tbag,prediction_raw_RF)
pred_bag_predictions_treebag<-predict(object=fit.treebag ,testing, type="raw")
prediction_raw_Tbag <-  prediction(as.numeric(pred_bag_predictions_treebag), testing$Rain)

pred_bag_predictions_rf<-predict(object=fit.rf ,testing, type="raw")
prediction_raw_RF <-  prediction(as.numeric(pred_bag_predictions_rf), testing$Rain)

pred_bag <- data.frame(pred_bag)

caTools::colAUC(prediction_raw_Tbag,prediction_raw_RF, testing$Rain, plotROC = TRUE)




###Boosting models#####
library(gbm)
set.seed(123)
kariki_gbm_fit <- gbm(Rain ~ .,data = training,n.trees = 10000,interaction.depth = 1,shrinkage = 0.001,distribution = "adaboost",cv.folds = 5,n.cores = NULL, verbose = FALSE)  

# With a a different distribution of bernouli or gaussian( square error) because rain is factor
# For bernoulli, it had an error using trees
kariki_gbm_fit_3 <- gbm(Rain ~ .,data = training,n.trees = 10000,interaction.depth = 1,shrinkage = 0.001,distribution = "gaussian",cv.folds = 5,n.cores = NULL, verbose = FALSE)  

print(kariki_gbm_fit)
# MSE and RMSE
sqrt(min(kariki_gbm_fit$cv.error))
sqrt(min(kariki_gbm_fit_3$cv.error))

#Plotting the loss function
gbm.perf(kariki_gbm_fit, method = "cv")
gbm.perf(kariki_gbm_fit_3, method = "cv")

# Tuning the model even further by reducing the no of trees abd the depth
kariki_gbm_fit_2 <- gbm(Rain ~ .,data = training,n.trees = 5000,interaction.depth = 3,shrinkage = 0.1,distribution = "adaboost",cv.folds = 5,n.cores = NULL, verbose = FALSE)  



########## Employing the GLM model on the data
trControl <- trainControl(method = "repeatedcv",  repeats = 5, number = 10, verboseIter = FALSE)
predictor_rain <-c("High_Temp","Avg_Temp","Low_Temp","Dewpoint_High","Dewpoint_Avg","Dewpoint_low","Humidity_High","Humidity_Avg","Humidity_Low","Windspeed_High","Windspeed_Avg")
prediction_formula <- as.formula(paste("Rain", paste(predictor_rain, collapse="+"), sep="~"))
kariki_ML_models <- train(prediction_formula,data = training,method = "glm",family="binomial", trControl = trControl, metric = 'Accuracy',maxit = 100)
kariki_ML_models$results$Accuracy
summary(kariki_ML_models) # From the summary of the model
