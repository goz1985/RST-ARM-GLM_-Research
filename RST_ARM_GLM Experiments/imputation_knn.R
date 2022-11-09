#### This I will use the imputation method to get the missing values and fill them up####################
library(RoughSets)
library(RoughSetKnowledgeReduction)
library(googlesheets4)
library(arules)
library(dplyr)
library(RColorBrewer)
library(arulesViz)
library(dplyr)
library(Metrics)
library(caret)
library(caretEnsemble)
library(ROCR)
library(classifierplots)

#Loading the dataset and preprocessing it
k_farm <- read_sheet("https://docs.google.com/spreadsheets/d/1y29ch-sv9UXSZUX9mRxqx6NleN6-XO3ifXDqkDzeOiE/edit#gid=0")

(n<-nrow(k_farm)) # Checking number of rows in the data which is 1619 observations with 17 varibales
c(as.character(k_farm$Date[1]), as.character(k_farm$Date[n])) # the date range from 2/3/2018 to 18/8/2022
head(k_farm$Rain)
k_farm$Rain <- factor(k_farm$Rain) # Rain Yes/No to factor    ordered = TRUE
k_farm$Date <- as.Date(k_farm$Date, '%m/%d/%Y') # Date column to date
str(k_farm)
(cols_withNa <- apply(k_farm, 2, function(x) sum(is.na(x))))

















#### Divide dataset into training and testing set, then impute using the K- nearest neighbor on the trainin set###
set.seed(123)
kariki_train <- createDataPartition(k_farm$Rain, p =0.75, list = FALSE)
training <- k_farm[kariki_train,]
testing <- k_farm[-kariki_train,]

# Create the knn imputation model on the training data
preProcess_missingdata_model <- preProcess(as.data.frame(training), method='knnImpute')
preProcess_missingdata_model

# The imputation method output means:
#' Centered(15): subtracted by mean of 15 variables
#' ignored 2
#' Used K=5 to predict missing values and sacled(standard deviation) of 15 variables in the training data


##### Imputing the whole dataset#####

kariki_imputed <- preProcess(as.data.frame(k_farm), method='knnImpute')
kariki_imputed

k_Names <- data.frame(col = names(kariki_imputed$mean), mean = kariki_imputed$mean, sd = kariki_imputed$std)
for (i in k_Names$col) 
  {
  
}

