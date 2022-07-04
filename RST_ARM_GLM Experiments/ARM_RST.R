# Dealing with KAriki farm using the rough set model and formulation of decision rules
# In this notebook, I will be using roughset theory on the kariki dataset to reduce the set of terms.
# Also I will want to see whether having a discretized dataset is better than having the original data.
# Which discretization methods are better

# The notebook will start by loading neccessary libraries, loading data, preprocessing it and removing null values
# Will also want to see whether I can do imputation on the data

library(RoughSets)
library(RoughSetKnowledgeReduction)
library(googlesheets4)
library(arules)
library(dplyr)

#Loading the dataset and preprocessing it
k_farm <- read_sheet("https://docs.google.com/spreadsheets/d/1y29ch-sv9UXSZUX9mRxqx6NleN6-XO3ifXDqkDzeOiE/edit#gid=0")

(n<-nrow(k_farm)) # Checking number of rows in the data which is 1179
c(as.character(k_farm$Date[1]), as.character(k_farm$Date[n])) # the date range from 2/3/2018 to 20/5/2021
head(k_farm$Rain)
k_farm$Rain <- factor(k_farm$Rain) # Rain Yes/No to factor
k_farm$Date <- as.Date(k_farm$Date, '%m/%d/%Y') # Date column to date
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

#### Removing the precipitation amount because it will influence the outcome variable#########
kariki_farm2$Precipitation_amount<- NULL
#############################################################################################################

########################## Rough-Sets Theory  ############################################################

kariki_shuffled <- kariki_farm2[sample(nrow(kariki_farm2)),]
kariki_table <- SF.asDecisionTable(kariki_shuffled,decision.attr = 14, indx.nominal = c(14))


#####################Discretze the elements in the data sets for both training and testing################
cut.values <- D.discretization.RST(kariki_train,type.method = "global.discernibility")  
kariki_discretized <- SF.applyDecTable(kariki_table,cut.values)

#######Feature selection using Greedy Heuristic Method#####################################################
kariki.rst_GH <- FS.greedy.heuristic.reduct.RST(train_discretized,qualityF = X.entropy,epsilon = 0.0)
GH_table_train <- SF.applyDecTable(kariki_discretized, kariki.rst_GH)
###########################################################################################################

####Feature selection uisng Quick reduct method###########################################################
kariki.rst_QR <- FS.feature.subset.computation(kariki_discretized,method="quickreduct.rst")
QR_Table_Train<- SF.applyDecTable(kariki_discretized, kariki.rst_QR)
###########################################################################################################


##########Both Quick reduct and Greedy-heuristic methods are giving me a reduct of the same 12 variables###########

############# Using the indiscernible and discernibility matrix to compute the reduct for the same aspect#####

system.time(IND <- BC.IND.relation.RST(kariki_discretized))

################################### Compute Upper and Lower Approximations#################################
rst_weather <- BC.LU.approximation.RST(kariki_discretized,IND)
print(rst_weather)

# Determining the regions
system.time(kariki.regions <- BC.positive.reg.RST(kariki_discretized,rst_weather))


##########Computations to display the upper and lower regions for the Roughset Model###################
low_app_no <- rst_weather$lower.approximation$`0`
upp_app_no <- rst_weather$upper.approximation$`0`
low_app_yes <- rst_weather$lower.approximation$`1`
upp_app_yes <- rst_weather$upper.approximation$`1`



boundary_app_yes <- setdiff(upp_app_yes,low_app_yes)
boundary_app_no <- setdiff(upp_app_no,low_app_no)

Uni_discourse <- c(1:nrow(kariki_discretized))
outer_region = setdiff(Uni_discourse,boundary_app_yes)


print(outer_region)
print(low_app_yes)
print(rst_weather)

########### Discernibility matrix and reduct formulation################################################################
system.time(kariki.matrix <- BC.discernibility.mat.RST(kariki_discretized,return.matrix = TRUE))
system.time(reduct_all <- FS.all.reducts.computation(kariki.matrix))
dec_table_1 <- SF.applyDecTable(kariki_discretized, reduct_all, control = list(indx.reduct = 1))
######### This method is giving me the same reduct as that of the Greedy heuristic, quick reduct methods######


# Analysis of the reduct using various Ml Models to compare with the other

############################Now applying several machine learning models on the data to see what it will output
library(tidyverse)
library(caret)
library(knitr)
library(lattice)
library(grid)
library(gridExtra)
library(ROCR)
library(corrplot)
library(caretEnsemble)
library(fastAdaboost)
library(gbm)
set.seed(123)

GH_table_train$Rain <- factor(GH_table_train$Rain, labels = c("No","Yes"))
GH_kariki_train <- createDataPartition(GH_table_train$Rain, p =0.75, list = FALSE)
GH_training <- GH_table_train[GH_kariki_train,]
GH_testing <- GH_table_train[-GH_kariki_train,]

#### Ensemble methods#########
GH_control <- trainControl(method="repeatedcv", number=10, repeats=3,
                        savePredictions=TRUE, classProbs=TRUE,preProc = c("center","scale"))

########Tree Bag Method, and running the testing data on the developed mode########################
########the model on the reduct gives an accuracy of 81%############################################
seed <- 123
metric <- "Accuracy"
set.seed(seed)
system.time(fit.treebag_GH <- train(Rain~., data=GH_training, method="treebag", metric=metric, trControl=GH_control))

predictions_treebag_GH<-predict(object=fit.treebag_GH ,GH_testing, type="raw")
table(predictions_treebag_GH)
confusionMatrix(predictions_treebag_GH,GH_testing$Rain)


#################### Running on the random forest model##############################################################
set.seed(seed)
system.time(fit.rf_GH <- randomForest(Rain~., data=GH_training,trControl=GH_control))
print(fit.rf_GH)
####The OOB(Out of Bag error) is 15.3% hence meaning that our models accuracy is around 84.7% on the training data####
predictions_rf_GH<-predict(object=fit.rf_GH ,GH_testing, type="response")
confusionMatrix(predictions_rf_GH,GH_testing$Rain)



####################################Running the GLM model Accuracy 82% ########################################
predictor_rain <-c("High_Temp","Dewpoint_High","Avg_Temp","Low_Temp","Dewpoint_low","Humidity_High","Humidity_Avg","Humidity_Low","Windspeed_High","Windspeed_Avg","High.Hpa.")
prediction_formula <- as.formula(paste("Rain", paste(predictor_rain, collapse="+"), sep="~"))
system.time(kariki_ML_models <- train(prediction_formula,data = GH_training,method = "glm",family="binomial", trControl = GH_control, metric = 'Accuracy',maxit = 100))
kariki_ML_models$results$Accuracy
summary(kariki_ML_models) # From the summary of the model

glm_responses <- predict(kariki_ML_models,GH_testing,type = "raw")
table(glm_responses)
confusionMatrix(glm_responses,GH_testing$Rain)


#####################Boosting ensemble method####################################################################

system.time(kariki_gbm <- gbm(Rain ~ .,data = GH_training,n.trees = 10000,interaction.depth = 1,shrinkage = 0.001,distribution = "gaussian",cv.folds = 5,n.cores = NULL, verbose = FALSE))
best.iter = gbm.perf(kariki_gbm, method="cv")# Number of iterations for best output
plot.gbm(kariki_gbm, 1, best.iter)

print(kariki_gbm)
summary(kariki_gbm)

predictions_gbm3<-predict(object=kariki_gbm ,GH_testing, type="link")
table(predictions_gbm3)
confusionMatrix(predictions_gbm3,GH_testing$Rain)
# MSE and RMSE
sqrt(min(kariki_gbm$cv.error)) # had an mse of 0.354436

#Plotting the loss function
gbm.perf(kariki_gbm, method = "cv")






####Applying the association rule mining method to get the decision rules


#Convert DT to transactions, this helps in identifying how attributes/features are related to each other
GH_Table_Frame <- as.data.frame(GH_table_train) # Coerce Decision table back to dataframe before coercing them to transactions
kariki_trans_GH<-as(GH_Table_Frame,"transactions")

itemLabels(kariki_trans_GH)
image(kariki_trans_GH)


#Generating the rule using apriori method, will generate only two rules, need to fine tune in step 149 to get more rules
kariki_rules <- apriori(kariki_trans_GH)

#Inspecting rules with the highest confidence
inspect(head(sort(kariki_rules, by='confidence'),5))


# Fine tuning the above function for deducing rules by tunning the support and confidence
kariki_rules_2 <- apriori(data=kariki_trans_GH, parameter=list (supp=0.01,conf =0.1, minlen= 2, maxtime=10, target = "rules"))
summary(kariki_rules_2)
# Need to get more quality rules from this reduct

# Rules formulated are only two rules

rules_df <- data.frame(lhs=labels(lhs(kariki_rules_2)),rhs=labels(rhs(kariki_rules_2)),kariki_rules_2@quality)
view(rules_df)

# Trying to switch it up a bit..remove precipitation aspect
GH_Table_Frame$Precipitation_amount<-NULL

# BAsically the reduct couldnt formulate rules..

#Generating the rule susing apriori method
kariki_rules <- apriori(kariki_trans_GH)






