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
kariki_farm <- read.csv("C:/Users/admin/Desktop/datasets/Kariki_Farm.csv")

#
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

######## Picking only complete values, I create another data frame kariki_farm2################
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

#Ensemble Models############### Tree Bag and RF#### Bagging
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


# Employing the GLM model on the data
trControl <- trainControl(method = "repeatedcv",  repeats = 5, number = 10, verboseIter = FALSE)
predictor_rain <-c("High_Temp","Avg_Temp","Low_Temp","Dewpoint_High","Dewpoint_Avg","Dewpoint_low","Humidity_High","Humidity_Avg","Humidity_Low","Windspeed_High","Windspeed_Avg")
prediction_formula <- as.formula(paste("Rain", paste(predictor_rain, collapse="+"), sep="~"))
kariki_ML_models <- train(prediction_formula,data = training,method = "glm",family="binomial", trControl = trControl, metric = 'Accuracy',maxit = 100)
kariki_ML_models$results$Accuracy
summary(kariki_ML_models) # From the summary of the model
