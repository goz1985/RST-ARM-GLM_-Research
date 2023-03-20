################ This script will use precipitation amount as the target variable################
library(caret)
library(caretEnsemble)
library(googlesheets4)
library(Metrics)
library(sjlabelled)
library(sjPlot)
library(flexmix) #Calculate BIC values
library(lmtest)
library(effects)
library(DALEX)
library(ranger)
library(modelStudio)
library(doParallel)  # for parallel backend to foreach
library(foreach)     # for parallel processing with for loops
library(rpart)
library(ipred)
library(dplyr)
library(RoughSets)
library(RoughSetKnowledgeReduction)
library(googlesheets4)
library(arules)
library(dplyr)
library(RColorBrewer)
library(arulesViz)

k_farm <- read_sheet("https://docs.google.com/spreadsheets/d/1y29ch-sv9UXSZUX9mRxqx6NleN6-XO3ifXDqkDzeOiE/edit#gid=0")
(n<-nrow(k_farm)) # Checking number of rows in the data which is 1619 observations with 17 varibales
c(as.character(k_farm$Date[1]), as.character(k_farm$Date[n])) # the date range from 2/3/2018 to 18/8/2022
head(k_farm$Rain)
k_farm$Rain <- factor(k_farm$Rain) # Rain Yes/No to factor    ordered = TRUE
k_farm$Date <- as.Date(k_farm$Date, '%m/%d/%Y') # Date column to date
str(k_farm)

view(k_farm)
(cols_withNa <- apply(k_farm, 2, function(x) sum(is.na(x))))

kariki_farm4 <- k_farm[complete.cases(k_farm),]
str(kariki_farm4)
(cols_withNa <- apply(kariki_farm4, 2, function(x) sum(is.na(x))))
kariki_farm4$Date <- NULL # removing the date column
kariki_farm4$Windspeed_low <- NULL
kariki_farm4$Rain<-NULL
plot(density(kariki_farm4$Precipitation_amount))

##############Modelling precipitation amount######################################################

set.seed(123)
k_farm <- createDataPartition(kariki_farm4$Precipitation_amount, p =0.75, list = FALSE)
k_train <- kariki_farm4[k_farm,]
k_test <- kariki_farm4[-k_farm,]
k_control <- trainControl(method="repeatedcv", number=10, repeats=3,
                         savePredictions=TRUE, classProbs=TRUE,preProc = c("center","scale"))


kariki_precipitation <-glm(Precipitation_amount~.,data = k_train,maxit = 100)
kariki_precipitation_prediction<-predict(object=kariki_precipitation ,k_train, type="terms")
table(kariki_precipitation_prediction)
mae(k_test$Precipitation_amount,predict(kariki_precipitation))
rmse(k_test$Precipitation_amount,predict(kariki_precipitation))
summary(kariki_precipitation)
anova(kariki_precipitation,test =c("Chisq"))
step_precp <- step(kariki_precipitation,direction = "both")


kariki_precipitation <-glm(Precipitation_amount~.,data = kariki_farm4,family = "poisson",maxit = 100)
summary(kariki_precipitation)
anova(kariki_precipitation)

kariki_rain_amount <- glm(Precipitation_amount~.,data = kariki_farm4,family = "Gamma"(link = log))


library(radiant)
library(timetk)

radiant()