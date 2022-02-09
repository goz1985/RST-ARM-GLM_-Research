#' In this scripts, I shall show how using RST technology aids in detecting interaction terms in a dataset
#' an d using these detected interaction terms in a glm model. Shall test various RST reduct formation methods
#' to check on their suitability for the work.

# First we shall load the required libraries and the dataset
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
kariki_farm <- read.csv("C:/Users/admin/Desktop/datasets/Kariki_Farm.csv")

# Preprocessing

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
view(kariki_farm2)

# Getting some insight on the new dataset and checking the correlation between the various feature attributes in the dataset
# Visualizing the correlation between the various variables in the dataset
# 1. There is a verystrong correlation between High.Hpa and Low.Hpa (positive) while a negative correlation between High_Temp and Humidity_low
# check the relationship between the following variables:
#' 1. Dew_Point High and Dew_point avg; Dew_Point High and Humidity High and Humidity_avg
#' 2. Positive correlation means increase in one varibale will lead to the increase in the other variable while negative means an increase will lead to a decrease in the other variable
factors_var <- names(which(sapply(kariki_farm2,class)=="factor"))
factors_var <-setdiff(factors_var,"Rain")

numeric_vars <- setdiff(colnames(kariki_farm2),factors_var)
numeric_vars <- setdiff(numeric_vars,"Rain")
numeric_vars


numeric_mat <- as.matrix(kariki_farm2[,numeric_vars,drop= FALSE])
numeric_cor <- cor(numeric_mat)
corrplot(numeric_cor)

# Modelling and training the kariki farm dataset on glm first
set.seed(123)
kariki_train <- createDataPartition(kariki_farm2$Rain, p =0.75, list = FALSE)
training <- kariki_farm2[kariki_train,]
testing <- kariki_farm2[-kariki_train,]

# First GLM model. We see when fitting the whole attributes in the dataset, the model just picks up all the attributes as being key for 
# rain in the model
glm1 <- glm(Rain ~., family = binomial, data = training, maxit = 100)
summary(glm1)

# Second glm model I picked only a subset of features which were Dewpoint_High ,Dewpoint_Avg + Dewpoint_low ,Humidity_High ,Humidity_Avg, Humidity_Low, Windspeed_High, Windspeed_Avg

glm2 <-glm(Rain ~ Dewpoint_High + Dewpoint_Avg + Dewpoint_low + Humidity_High + Humidity_Avg + Humidity_Low + Windspeed_High + Windspeed_Avg, family = binomial, data = training, maxit = 100)
summary(glm2)

# Third GLM model I picked a subset pf variables containing High_Temp, Avg_Temp, Low_Temp
# With this we see we cant differentiate between High, Low and Avg temp as being important predictors for RAin
glm3 <- glm(Rain ~ High_Temp + Avg_Temp + Low_Temp, family = binomial, data = training, maxit = 100)
summary(glm3)


# using the train control method, I modeled the Kariki data on the glm model and got an accuracy of 83% on the model
# From the glm summary we see the important attributes are High Temp;Dew_point high,wind speed high and average
trControl <- trainControl(method = "repeatedcv",  repeats = 5, number = 10, verboseIter = FALSE)
predictor_rain <-c("High_Temp","Avg_Temp","Low_Temp","Dewpoint_High","Dewpoint_Avg","Dewpoint_low","Humidity_High","Humidity_Avg","Humidity_Low","Windspeed_High","Windspeed_Avg")
prediction_formula <- as.formula(paste("Rain", paste(predictor_rain, collapse="+"), sep="~"))
kariki_ML_models <- train(prediction_formula,data = kariki_farm2,method = "glm",family="binomial", trControl = trControl, metric = 'Accuracy',maxit = 100)
kariki_ML_models$results$Accuracy
summary(kariki_ML_models) # From the summary of the model

# Used the adaboost and decision tree model..install the packages first for the adaboost model

kariki_ML_models_2 <- train(prediction_formula,data = kariki_farm2,method = "adaboost",trControl = trControl, metric = 'Accuracy',maxit = 100)
kariki_ML_models_2$results$Accuracy
summary(kariki_ML_models_2)

kariki_ML_models_3 <- train(prediction_formula,data = kariki_farm2,method = "C5.0Tree",trControl = trControl, metric = 'Accuracy',maxit = 100)
kariki_ML_models_3$results$Accuracy
summary(kariki_ML_models_3)

########### Roughset##########
# First shuffle then discretize the values
kariki_shuffled <- kariki_farm2[sample(nrow(kariki_farm2)),]
kariki_DT <- SF.asDecisionTable(kariki_shuffled, decision.attr = 16, indx.nominal = 16)
kariki_DT_cutValues <- D.discretization.RST(kariki_DT, type.method = "global.discernibility")
kariki_Table_Discretized <- SF.applyDecTable(kariki_DT,kariki_DT_cutValues)

# Then an indiscernibility relation is deduced inorder to get the attributes that are indiscernable
kariki_IND <- BC.IND.relation.RST(kariki_Table_Discretized)
roughset <- BC.LU.approximation.RST(kariki_Table_Discretized,kariki_IND)

# Use the discernibility matrix, a reduct is generated with 3 attributes remaining that is windspeed avg, precipitation amount and high pressure
# Funny that the High.Hpa is discerned as a key attribute in relation to rain
regions.rst <- BC.positive.reg.RST(kariki_Table_Discretized,roughset)
disc.mat <- BC.discernibility.mat.RST(kariki_Table_Discretized)
reduct <- FS.all.reducts.computation(disc.mat)
new.decTable <- SF.applyDecTable(kariki_Table_Discretized, reduct, control = list(indx.reduct = 1))

# Applying this reduct on the GLM model...giving me an accuracy of 67%
trControl_2 <- trainControl(method = "repeatedcv",  repeats = 5, number = 10, verboseIter = FALSE)
predictor_rain_2 <-c("High.Hpa.","Windspeed_Avg")
prediction_formula_2 <- as.formula(paste("Rain", paste(predictor_rain_2, collapse="+"), sep="~"))
kariki_ML_models_2 <- train(prediction_formula_2,data = new.decTable,method = "glm",family="binomial", trControl = trControl, metric = 'Accuracy',maxit = 100)
kariki_ML_models_2$results$Accuracy
summary(kariki_ML_models_2) # From the summary of the model

## In relation to rain
#' When the pressure is low, the air is free to rise into the atmosphere where it cools and condenses. ...
#'  Eventually the water vapor in the clouds condenses and falls as rain.


# How can I improve on the above model?? On 12/11/2021 I saw that using the RST discretization methods
# of global discernibility wasnt giving good values......

# Generate Rules using both ARM and RST methods and then compare the rules interms of confidence and support

# Using The quick reduct method to generate a reduct and compare the attributes with the one with all reduct computation
kariki_shuffled <- kariki_farm2[sample(nrow(kariki_farm2)),]
kariki_DT <- SF.asDecisionTable(kariki_shuffled, decision.attr = 16, indx.nominal = 16)
kariki_DT_cutValues <- D.discretization.RST(kariki_DT, type.method = "local.discernibility")
kariki_Table_Discretized <- SF.applyDecTable(kariki_DT,kariki_DT_cutValues)

# Feature selection. This reduct using Quick reduct generates 6 attributes which are discernable and have an ultimate effect on the decision variable rain
# The attributes are: Avg_temp, DewPoint_Avg,Dewpoint_Low,Windspeed_Avg,Precipitation_Amount

kariki.rst <- FS.feature.subset.computation(kariki_Table_Discretized,method="quickreduct.rst")
kariki_QR<- SF.applyDecTable(kariki_DT, kariki.rst)

#Reduct generation using greedy-heursitic method
# COde aborts:.....Was doing it the wrong way, the table i used was the one which was discretized..Have to discretize it firts before I forumulate the reduct with greedy heuristic
# This reduct also has the same attributes as the Quick Reduct method
GHR_RST <-  FS.greedy.heuristic.reduct.RST(kariki_Table_Discretized, qualityF = X.entropy,epsilon = 0.0)
GHR_Reduct <- SF.applyDecTable(kariki_DT,GHR_RST)
