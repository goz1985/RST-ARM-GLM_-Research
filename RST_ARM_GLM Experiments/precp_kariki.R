# This script seeks to analyze the kariki farm dataset in relation to precipitation amount. Also will look into using RST 
# to detect interaction terms then use association rule analysis to generate decision rules then generate binary values for the rules.
library(RoughSets)
library(RoughSetKnowledgeReduction)
library(googlesheets4)
library(arules)
library(dplyr)
library(RColorBrewer)
library(arulesViz)

#Loading the dataset and preprocessing it
k_farm <- read_sheet("https://docs.google.com/spreadsheets/d/1y29ch-sv9UXSZUX9mRxqx6NleN6-XO3ifXDqkDzeOiE/edit#gid=0")
(n<-nrow(k_farm)) # Checking number of rows in the data which is 1619 observations with 17 varibales
c(as.character(k_farm$Date[1]), as.character(k_farm$Date[n])) # the date range from 2/3/2018 to 18/8/2022
head(k_farm$Rain)
k_farm$Rain <- factor(k_farm$Rain) # Rain Yes/No to factor    ordered = TRUE
k_farm$Date <- as.Date(k_farm$Date, '%m/%d/%Y') # Date column to date
str(k_farm)

(cols_withNa <- apply(k_farm, 2, function(x) sum(is.na(x))))# Getting missing value tabulation

weather_juja <- k_farm[complete.cases(k_farm),]
str(weather_juja)
(cols_withNa <- apply(weather_juja, 2, function(x) sum(is.na(x))))
weather_juja$Date <- NULL # removing the date column
weather_juja$Windspeed_low <- NULL
weather_juja$Rain<-NULL


# Dividing dataset into training and testing set
library(dplyr)
library(Metrics)
library(caret)
library(caretEnsemble)
library(Metrics)
set.seed(123)
juja_weather<- createDataPartition(weather_juja$Precipitation_amount, p =0.75, list = FALSE)
juja_training <- weather_juja[juja_weather,]
juja_testing <- weather_juja[-juja_weather,]

control <- trainControl(method="repeatedcv", number=10, repeats=3,
                        savePredictions=TRUE, classProbs=TRUE,preProc = c("center","scale"))

model_fit <- train(Precipitation_amount~., data = juja_training,method = "glm", family= "gaussian")
summary(model_fit)
exp(coef(model_fit$finalModel))
model_fit$results
### Doing prediction
model_predict <- predict(model_fit,newdata = juja_testing, type = "raw")
postResample(pred = model_predict, obs = juja_testing$Precipitation_amount) # USing this to calculate RMSE, R squared and MAE



##################################### Applying roughset to the dataset########################################

juja_shuffled <- weather_juja[sample(nrow(weather_juja)),]
juja_table <- SF.asDecisionTable(juja_shuffled,decision.attr = 14, indx.nominal = c(14))

#####################################Discretize the elements in the data sets ################################
juja.cut.values <- D.discretization.RST(juja_table,type.method = "local.discernibility")  
juja_discretized <- SF.applyDecTable(juja_table,juja.cut.values)


####################################Using indiscernibility relation to deduce a reduct of important variables######
system.time(juja.IND <- BC.IND.relation.RST(juja_discretized))

################################### Compute Upper and Lower Approximations######################################
juja.weather.rst <- BC.LU.approximation.RST(juja_discretized,juja.IND)
print(juja.weather.rst)

#####################################Determining the regions####################################################
system.time(juja.regions <- BC.positive.reg.RST(juja_discretized,juja.weather.rst))

####################################Computations to display the upper and lower regions for the Roughset Model###################
low_no <- juja.weather.rst$lower.approximation$`0`
upp_no <- juja.weather.rst$upper.approximation$`0`
low_yes <- juja.weather.rst$lower.approximation$`1`
upp_yes <- juja.weather.rst$upper.approximation$`1`



boundary_yes <- setdiff(upp_yes,low_yes)
boundary_no <- setdiff(upp_no,low_no)

Uni_discourse <- c(1:nrow(juja_discretized))
outer_region = setdiff(Uni_discourse,boundary_yes)
print(outer_region)
print(low_yes)
print(juja.weather.rst)

######################################## Discernibility matrix and reduct formulation################################################################
system.time(juja.matrix <- BC.discernibility.mat.RST(juja_discretized,return.matrix = TRUE))
system.time(juja_reduct_all <- FS.all.reducts.computation(juja.matrix))
juja_Precp_reduct <- SF.applyDecTable(juja_discretized, juja_reduct_all, control = list(indx.reduct = 1))


######################################## Fitting GLM model to the reduct###################################
set.seed(123)
metric<-"Accuracy"
juja_Reduct_weather<- createDataPartition(juja_Precp_reduct$Precipitation_amount, p =0.75, list = FALSE)
juja_Reduct_training <- juja_Precp_reduct[juja_Reduct_weather,]
juja_Reduct_testing <- juja_Precp_reduct[-juja_Reduct_weather,]

Reduct_control <- trainControl(method="repeatedcv", number=10, repeats=3,
                        savePredictions=TRUE, classProbs=TRUE,preProc = c("center","scale"))

model_fit2 <- train(Precipitation_amount~., data = juja_Reduct_training, method = "glm",family= "gamma")
###########################################Bringing an error#####################################################################


##################################Using the greedy heuristic method##########################################################
juja_GH_reduct <- FS.greedy.heuristic.reduct.RST(juja_discretized,qualityF = X.entropy,epsilon = 0.1)
juja.GH.Reduct<- SF.applyDecTable(juja_table, juja_GH_reduct)

############################The greedy heuristic reduct has 8 important features##################################################

################Fitting it to a GLM model with the gaussian family as link###############################
set.seed(123)
metric<-"RMSE"
juja_GH_weather<- createDataPartition(juja.GH.Reduct$Precipitation_amount, p =0.75, list = FALSE)
juja_GH_training <- juja.GH.Reduct[juja_GH_weather,]
juja_GH_testing <- juja.GH.Reduct[-juja_GH_weather,]

GH_Reduct_control <- trainControl(method="repeatedcv", number=10, repeats=3,
                               savePredictions=TRUE, classProbs=TRUE,preProc = c("center","scale"))

model_fit3 <- train(Precipitation_amount~., data = juja_GH_training,method = "glm", family= "gamma")
model_fit3<-lm(Precipitation_amount~., data = juja_GH_training,maxit = 100)