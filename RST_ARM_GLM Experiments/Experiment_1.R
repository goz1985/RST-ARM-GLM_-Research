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
# Data loading and preprocessing: Using Kariki farm and Kirogo site weather data.
kariki_farm <- read.csv("D:/datasets/Kariki_Farm1.csv", stringsAsFactors = FALSE, fileEncoding="latin1")
view(kariki_farm)
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
(cols_withNa <- apply(kariki_farm, 2, function(x) sum(is.na(x)))) # checking for columns with missing values we see that
#' apart from Date, Low and High Hpa and Rain all the other columnns have missing values.

######## Picking only complete values, I create another data frame kariki_farm2################
kariki_farm2 <- kariki_farm[complete.cases(kariki_farm),]
str(kariki_farm2)
(cols_withNa <- apply(kariki_farm2, 2, function(x) sum(is.na(x))))
kariki_farm2$Date <- NULL # removing the date column
kariki_farm2$Rain <- NULL # removing the Rain Decision factor column
view(kariki_farm2)

factors_var <- names(which(sapply(kariki_farm2,class)=="factor"))
factors_var <-setdiff(factors_var,"Precipitation_amount")

numeric_vars <- setdiff(colnames(kariki_farm2),factors_var)
numeric_vars <- setdiff(numeric_vars,"Rain")
numeric_vars

# Visualizing the correlation between the various variables in the dataset
# 1. There is a verystrong correlation between High.Hpa and Low.Hpa (positive) while a negative correlation between High_Temp and Humidity_low
# check the relationship between the following variables:
#' 1. Dew_Point High and Dew_point avg; Dew_Point High and Humidity High and Humidity_avg
#' 2. Positive correlation means increase in one varibale will lead to the increase in the other variable while negative means an increase will lead to a decrease in the other variable
numeric_mat <- as.matrix(kariki_farm2[,numeric_vars,drop= FALSE])
numeric_cor <- cor(numeric_mat)
corrplot(numeric_cor)
corrplot(numeric_cor,type = "upper")# viewing the upper part of the corrplot
corrplot(numeric_cor,type = "lower")# viewing the lower part of the corrplot

# First density plots are for evaluation against the variable if it will rain or not
kariki_dp <- invisible(lapply(numeric_vars, function(x) { 
  ggplot(data=kariki_farm2, aes(x=eval(parse(text=x)), col = Rain)) + geom_density() + xlab(x) + ggtitle(paste(x, "density", sep= " "))}))
kariki_dp

# Second density plot is for evaluation of precipitation_amount against other prameters..of importance are the humidity high;
kariki_dp_2 <- invisible(lapply(numeric_vars, function(x) { 
  ggplot(data=kariki_farm2, aes(x=eval(parse(text=x)), col = Precipitation_amount)) + geom_density() + xlab(x) + ggtitle(paste(x, "density", sep= " "))}))
kariki_dp_2

# Modelling and training the kariki farm dataset on glm first
set.seed(123)
kariki_train <- createDataPartition(kariki_farm2$Rain, p =0.75, list = FALSE)
training <- kariki_farm2[kariki_train,]
testing <- kariki_farm2[-kariki_train,]

# Fitting two glm models with different attributes to check which is viabale to do the analysis, because I was looking at rain I used the binomial link function
glm3 <- glm(Rain ~ High_Temp + Avg_Temp + Low_Temp, family = binomial, data = training, maxit = 100)
summary(glm3)

glm4 <-glm(Rain ~ Dewpoint_High + Dewpoint_Avg + Dewpoint_low + Humidity_High + Humidity_Avg + Humidity_Low + Windspeed_High + Windspeed_Avg, family = binomial, data = training, maxit = 100)
summary(glm4)


#Computing the residual deviance and residual degrees of freedom
c( "Dev(m0)"= deviance( glm3 ), "Dev(m1)" = deviance( glm4 ) ) # deviance
c( "df(m0)" = df.residual( glm3 ), "df(m1)" = df.residual( glm4 ) )

#comparing the two models
L <- deviance( glm3 ) - deviance( glm4 ); L

pchisq(L, df.residual(glm3) - df.residual(glm4), lower.tail=FALSE ) # It shows it is important to consider all the variables in the glm model has a great overall importance on the model


###Applying the train control(cross validation method)########### change data from training to kariki_farm2#######
modelnames <- paste(names(getModelInfo()), collapse=',  ')# list of all training models in caret
modelnames


trControl <- trainControl(method = "repeatedcv",  repeats = 5, number = 10, verboseIter = FALSE)
predictor_rain <-c("High_Temp","Avg_Temp","Low_Temp","Dewpoint_High","Dewpoint_Avg","Dewpoint_low","Humidity_High","Humidity_Avg","Humidity_Low","Windspeed_High","Windspeed_Avg")
prediction_formula <- as.formula(paste("Rain", paste(predictor_rain, collapse="+"), sep="~"))
kariki_ML_models <- train(prediction_formula,data = kariki_farm2,method = "glm",family="binomial", trControl = trControl, metric = 'Accuracy',maxit = 100)
kariki_ML_models$results$Accuracy
summary(kariki_ML_models) # From the summary of the model

kariki_ML_models_2 <- train(prediction_formula,data = kariki_farm2,method = "adaboost",trControl = trControl, metric = 'Accuracy',maxit = 100)
kariki_ML_models_2$results$Accuracy
summary(kariki_ML_models_2)

kariki_ML_models_3 <- train(prediction_formula,data = kariki_farm2,method = "C5.0Tree",trControl = trControl, metric = 'Accuracy',maxit = 100)
kariki_ML_models_3$results$Accuracy
summary(kariki_ML_models_3)

##################CODE HAS ISSUE TO GET SOME STATISTICS FROM THE PREDICTION###################################################################################
# Compared confidence intervals for the two glm models after prediction(line 60 to 62); then form the confidence interval for the mean by using the inverse logarithimic function (line 63 to)
predict_model3 <-predict(glm3, newdata = testing, type = "response" ,se.fit = TRUE)
predict_model4 <- predict(glm4, newdata = testing, type = "response" ,se.fit= TRUE)
c( exp( predict_model3$fit ), predict_model4$fit )
kstar <- qnorm(p=0.975) # 95% confidence interval
model3_lo<-exp(predict_model3$fit - kstar*predict_model3$se.fit)
model3_hi <- exp(predict_model3$fit + kstar*predict_model3$se.fit)
c(lower = model3_lo, Estimate = exp(predict_model3$fit), Upper = model3_hi )
c( model3_lo-exp(predict_model3$fit), model3_hi-exp(predict_model3$fit))
###CHECK ABOVE CODE FROM PDF 7.2.3)######## there is an issue#################################################################################################

##############################################################################################################################################################

# Now fitting the data to a RST model
#########WRONG################################################ TO CHECK IT#####
kariki_shuffled <- kariki_farm2[sample(nrow(kariki_farm2)),]

split <- round(0.75*nrow(kariki_shuffled))
kariki.train <- SF.asDecisionTable(kariki_shuffled[1:split,],decision.attr = 15, indx.nominal = 15)
kariki.test <- SF.asDecisionTable(kariki_shuffled[(split + 1): nrow(kariki_shuffled), -ncol(kariki_shuffled)])

kariki_cut_values <- D.discretization.RST(kariki.train, type.method = "local.discernibility")
kariki_train <- SF.applyDecTable(kariki.train,kariki_cut_values)
kariki_test <- SF.applyDecTable(kariki.test,kariki_cut_values)

# Feature selection
kariki.rst <- FS.feature.subset.computation(kariki_train,method="quickreduct.rst")
kariki.tra <- SF.applyDecTable(kariki_train, kariki.rst)
##################WRONG#####################################TO CHECK IT########


####### Before splitting#############################

kariki_shuffled <- kariki_farm2[sample(nrow(kariki_farm2)),]
kariki_DT <- SF.asDecisionTable(kariki_shuffled, decision.attr = 16, indx.nominal = 16)
kariki_DT_cutValues <- D.discretization.RST(kariki_DT, type.method = "global.discernibility")
kariki_Table_Discretized <- SF.applyDecTable(kariki_DT,kariki_DT_cutValues)
kariki_IND <- BC.IND.relation.RST(kariki_Table_Discretized)
roughset <- BC.LU.approximation.RST(kariki_Table_Discretized,kariki_IND) # Large element

regions.rst <- BC.positive.reg.RST(kariki_Table_Discretized,roughset)
disc.mat <- BC.discernibility.mat.RST(kariki_Table_Discretized)
reduct <- FS.all.reducts.computation(disc.mat)
new.decTable <- SF.applyDecTable(kariki_Table_Discretized, reduct, control = list(indx.reduct = 1))

# Spliting the new.dectable reduct on a threashold of 75% training..25% testing##########

set.seed(123)
kariki_train <- createDataPartition(new.decTable$Rain, p =0.75, list = FALSE)
training <- new.decTable[kariki_train,]
testing <- new.decTable[-kariki_train,]


#######Rule generation using RST methods###################################################

Kariki_rules_RI<-RI.indiscernibilityBasedRules.RST(new.decTable,reduct)
Kariki_rules_LEM <- RI.LEM2Rules.RST(wine_GHR_Table)
Kariki_rules_AQR <- RI.AQRules.RST(wine_GHR_Table)







#Fitting a glm model on this reduct
glm5 <- glm(Rain ~ Humidity_High + Windspeed_Avg + High.Hpa., family = binomial, data = kariki.train, maxit = 100)
summary(glm5)
confint(glm5)
plot1 <-cdplot(Rain ~ Humidity_High, data = kariki.train)
plot2 <- cdplot(Rain ~ Windspeed_Avg, data = kariki.train)
plot3 <- cdplot(Rain ~ High.Hpa., data = kariki.train)
glm6 <- glm(Precipitation_amount ~ Humidity_High + Windspeed_Avg + High.Hpa., family = gaussian, data = kariki.train, maxit = 100 )# I need to ensure when setting up my RSt model, I set precipitation as the decision variable

#####Rule generation using Arules.....Not set the decision attribute
library(arules)
kariki_dataframe <- data.frame(new.decTable)
karikidata <- as(kariki_dataframe,"transactions")
rules_2 <- apriori(karikidata,parameter = list(minlen = 2,supp= 0.005, conf = 0.8),appearance = list(rhs= c("Rain=1", "Rain=0")))
summary(rules_2)
rules_dataframe_2 <- data.frame(DATAFRAME(rules_2, separate = TRUE, setStart = '', itemSep = ' + ', setEnd = ''))
# rules_dt_2 <- data.table( lhs = labels( lhs(rules_2) ), rhs = labels( rhs(rules_2) ),   quality(rules_2) )[ order(-lift), ]


rules <- apriori(kariki_dataframe,parameter = list(minlen=2,supp=0.005,conf=0.8),appearance = list(rhs=c("Rain=1","Rain=0")))
summary(rules)
rules_dataframe <- data.frame(DATAFRAME(rules, separate = TRUE, setStart = '', itemSep = ' + ', setEnd = ''))

# rules_dt <- data.table( lhs = labels( lhs(rules) ), rhs = labels( rhs(rules) ),   quality(rules) )[ order(-lift), ]

