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
library(RColorBrewer)
library(arulesViz)
library(mosaic)

gs4_deauth()
k_link<-"https://docs.google.com/spreadsheets/d/1y29ch-sv9UXSZUX9mRxqx6NleN6-XO3ifXDqkDzeOiE/edit?usp=sharing"
k_farm<-read_sheet(k_link)


(n<-nrow(k_farm)) # Checking number of rows in the data which is 1619 observations with 17 varibales
c(as.character(k_farm$Date[1]), as.character(k_farm$Date[n])) # the date range from 2/3/2018 to 18/8/2022
head(k_farm$Rain)
k_farm$Rain <- factor(k_farm$Rain) # Rain Yes/No to factor    ordered = TRUE
k_farm$Date <- as.Date(k_farm$Date, '%m/%d/%Y') # Date column to date
str(k_farm)

view(k_farm)
(cols_withNa <- apply(k_farm, 2, function(x) sum(is.na(x))))# Getting missing value tabulation

######## Picking only complete values, I create another data frame kariki_farm2################I removed missing value rows in the dataset
kariki_farm2 <- k_farm[complete.cases(k_farm),]
str(kariki_farm2)
(cols_withNa <- apply(kariki_farm2, 2, function(x) sum(is.na(x))))
kariki_farm2$Date <- NULL # removing the date column
kariki_farm2$Windspeed_low <- NULL # removing windspeed low as it has no effect on the prediction 
kariki_farm2$Rain <- factor(kariki_farm2$Rain, labels = c("No","Yes"))
str(kariki_farm2)
#### Removing the precipitation amount because it will influence the outcome variable#########
kariki_farm2$Precipitation_amount<- NULL
#########################################Splitting into training and testing sets, then carrying out a cross validation on the same####################################################################
library(dplyr)
library(Metrics)
library(caret)
library(caretEnsemble)
set.seed(123)
kariki_train <- createDataPartition(kariki_farm2$Rain, p =0.75, list = FALSE)
training <- kariki_farm2[kariki_train,]
testing <- kariki_farm2[-kariki_train,]

control <- trainControl(method="repeatedcv", number=10, repeats=20,summaryFunction = twoClassSummary,
                        savePredictions=TRUE, classProbs=TRUE,preProc = c("center","scale"))

######Applying The GLM model on the data before using RST to get the interaction terms from the data set..84% accuracy##########

predictor_rain <-c("High_Temp","Avg_Temp","Low_Temp","Dewpoint_High","Dewpoint_Avg","Dewpoint_low","Humidity_High","Humidity_Avg","Humidity_Low","Windspeed_High","Windspeed_Avg")
prediction_formula <- as.formula(paste("Rain", paste(predictor_rain, collapse="+"), sep="~"))
kariki_ML <- train(prediction_formula,data = training,method = "glm",family="binomial", trControl = control, metric = 'Accuracy',maxit = 100)
kariki_ML$results
kariki_ML$finalModel
summary(kariki_ML)
kariki_ML$metric
exp(coef(kariki_ML$finalModel))
confint(kariki_ML)

printCoefmat( coef( summary(kariki_ML) ) ) #Wald test for single regression coefficients
#confint(kariki_ML)
glm_Org <- predict(kariki_ML,testing,type = "raw")
table(glm_Org)
confusionMatrix(glm_Org,testing$Rain)







library(ROCR)
roc_pred <- prediction(predictions = glm_Org[,2],testing$Rain)
plot(roc_pred)

library(classifierplots)
glm_prob_predict <- predict(kariki_ML,testing,type = "prob")
confusionMatrix(glm_prob_predict,testing$Rain)
colnames(glm_prob_predict)<-c("1","0")
classifierplots::density_plot(testing$Rain, glm_prob_predict$`0`)


printCoefmat( coef( summary(kariki_ML) ) ) ## We see from the coefficient summary we see Low Temp, DewPoint_high,Windspeed_High and Windspee_Avg are important predictors (Evidence for the P-values)..
##https://www.cell.com/trends/ecology-evolution/fulltext/S0169-5347(21)00284-6?_returnURL=https%3A%2F%2Flinkinghub.elsevier.com%2Fretrieve%2Fpii%2FS0169534721002846%3Fshowall%3Dtrue######

# Step wise regression to determine important prediction attributes
kariki_glm <- glm(Rain~., data = training,family = "binomial",maxit = 100)
kariki_predict <- predict(kariki_glm, testing,type = "response")
rain_or_no_rain <- ifelse(kariki_predict > 0.5,0,1)
rain_class <- factor(rain_or_no_rain, levels = levels(testing$Rain))

confusionMatrix(rain_class, testing[["Rain"]])

step_kariki <- step(kariki_glm,direction = "both")
glm_pred <- ROCR::prediction(glm_Org,testing$Rain)

#####Another method for GLM#######

kariki_fit <-caret::train(training,testing,method= "glm",trControl = control, metric = 'Accuracy', maxit = 100)






########################## Rough-Sets Theory  ############################################################

kariki_shuffled <- kariki_farm2[sample(nrow(kariki_farm2)),]
kariki_table <- SF.asDecisionTable(kariki_shuffled,decision.attr = 14, indx.nominal = c(14))


#####################Discretize the elements in the data sets for both training and testing################
cut.values <- D.discretization.RST(kariki_table,type.method = "global.discernibility")  
kariki_discretized <- SF.applyDecTable(kariki_table,cut.values)

#######Feature selection using Greedy Heuristic Method where it computes the approximate reduct#####################################################
GH_2_RST <- FS.greedy.heuristic.reduct.RST(kariki_discretized,qualityF = X.entropy,epsilon = 0.1)
Kariki_Reduct2 <- SF.applyDecTable(kariki_table, GH_2_RST)
###########################################################################################################


#######Feature selection using Greedy Heuristic Method where it does not computes the approximate reduct#####################################################
GH_3_RST <- FS.greedy.heuristic.reduct.RST(kariki_discretized,qualityF = X.entropy,epsilon = 0.0)
Kariki_Reduct_3 <- SF.applyDecTable(kariki_table, GH_3_RST)
###########################################################################################################


#############################Greedy Heuristic method GH_1 when applying attrDescriptions to the reduct generation process######################
### The epsilon =0.0; the quality F= X.entropy uses information gain to calculate the most important attributes#####

desc.attr <- c(1,2,3,4,5,6,7,8,9,10,11,12,13)
GH_1 <- FS.greedy.heuristic.reduct.RST(kariki_discretized,attrDescriptions = attr(kariki_discretized,"desc.attr"),qualityF = X.entropy,epsilon = 0.1)
Kariki_Reduct1<-SF.applyDecTable(kariki_table,GH_1)




#' For feature selection using Greedy heuristic method: Notes
#' 1. Kariki_Reduct 1 and Kariki_reduct 2 give the same attributes after setting the epsilon value to 1 and giving the RST method a set of attributes to check.
#' 2. Kariki_Reduct3 had 14 variables as it didn't compute the approximate reduct. For starters I find High_temp and Avg_temp almost having the same values

write.csv(GH_table_train,"D:\\Phd Research\\reduct.csv", row.names = FALSE)



write.csv(Kariki_Reduct2,"D:\\Phd Research\\GHreduct_2.csv", row.names = FALSE)



####Feature selection uisng Quick reduct method###########################################################
kariki.rst_QR <- FS.feature.subset.computation(kariki_discretized,method="quickreduct.rst")
QR_Table<- SF.applyDecTable(kariki_table, kariki.rst_QR)
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
print(upp_app_no)
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
library(randomForest)
set.seed(123)

Kariki_Reduct2$Rain <- factor(Kariki_Reduct2$Rain, labels = c("No","Yes"))
GH_kariki_train <- createDataPartition(Kariki_Reduct2$Rain, p =0.75, list = FALSE)
GH_training <- Kariki_Reduct2[GH_kariki_train,]
GH_testing <- Kariki_Reduct2[-GH_kariki_train,]

#### Ensemble methods#########
GH_control <- trainControl(method="repeatedcv", number=10, repeats=3,
                        savePredictions=TRUE, classProbs=TRUE,preProc = c("center","scale"))

########Tree Bag Method, and running the testing data on the developed mode########################
########the model on the reduct gives an accuracy of 83%############################################
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
####The OOB(Out of Bag error) is 15.3% hence meaning that our models accuracy is around 85.6% on the training data####
predictions_rf_GH<-predict(object=fit.rf_GH ,GH_testing, type="response")
confusionMatrix(predictions_rf_GH,GH_testing$Rain)



####################################Running the GLM model Accuracy 83% using caret package ########################################
predictor_rain <-c("Dewpoint_High","High_Temp","Low_Temp","Humidity_High","Windspeed_High","Windspeed_Avg","High.Hpa.")
prediction_formula_GH <- as.formula(paste("Rain", paste(predictor_rain, collapse="+"), sep="~"))
system.time(kariki_RST_GH_model <- train(prediction_formula_GH,data = GH_training,method = "glm",family="binomial", trControl = GH_control, metric = 'Accuracy',maxit = 100))
kariki_RST_GH_model$results$Accuracy
summary(kariki_RST_GH_model) # From the summary of the model
anova(kariki_RST_GH_model,test = 'Chisq')

glm_responses_GH <- predict(kariki_RST_GH_model,GH_testing,type = "raw")
table(glm_responses_GH)
confusionMatrix(glm_responses_GH,GH_testing$Rain)

################################################Using GLM model package#########################
kariki_GH_Logit<-glm(Rain~.,data = GH_training,family = "binomial",maxit = 100)
summary(kariki_GH_Logit) # From the summary of the model
anova(kariki_GH_Logit,test = 'Chisq')## We see Humidty_high and Wind_speed_high are not significant for prediction of Rain, while the rest are.
with(summary(kariki_GH_Logit),1-deviance/null.deviance)#R Squared value is 0.434

#####################Boosting ensemble method####################################################################

system.time(kariki_gbm <- gbm(Rain ~ .,data = GH_training,n.trees = 10000,interaction.depth = 1,shrinkage = 0.001,distribution = "gaussian",cv.folds = 5,n.cores = NULL, verbose = FALSE))
best.iter = gbm.perf(kariki_gbm, method="cv")# Number of iterations for best output
plot.gbm(kariki_gbm, 1, best.iter)

print(kariki_gbm)
summary(kariki_gbm)

predictions_gbm3<-predict(object=kariki_gbm ,GH_testing, type="link")
table(predictions_gbm3)
confusionMatrix(predictions_gbm3,GH_testing$Rain)### Giving now an error
# MSE and RMSE
sqrt(min(kariki_gbm$cv.error)) # had an mse of 0.3390606

#Plotting the loss function, the best number of trees to use here was 9991
gbm.perf(kariki_gbm, method = "cv")


#' In my first run of using the Roughset reduct, I used used the discretized table to generate the reducts, the reduct had Avg_Humidity as one of its predictors
#' which performed relatively well as they had the accuracy of
#' Treebag model had 81%
#' Random forest had 85%
#' GLM had 82 % this is because the table had discretized cut values
#' Now when using the set decision table(kariki_table) to generate the reduct, it had no Humidity_Avg as one of its predictors
#' Treebag had 83 %
#' Random forest had 86%
#' GLM had 84%
#' Boosting... will have to check it had an issue with generating the confusion matrix but the mse was 0.3390606
#' Number of trees was 9991, for the reduct with discretized values it was 10000 trees with an mse of 0.35




# Comparing the two GLM models to see which fit the best
anova(kariki_ML,kariki_RST_GH_model,test ="Chisq")

library(lmtest)
lrtest(kariki_ML,kariki_RST_GH_model)




#########Using the fun modelling to discretize the precipitation amount if to use it in my work###################
GH_bins <- discretize_get_bins(data = GH_table_train,input = "Precipitation_amount", n_bins = 10)

# Now it can be applied on the same data frame or in a new one (for example, in a predictive model that changes data over time)
GH_discretized=discretize_df(data=GH_table_train, data_bins=GH_bins, stringsAsFactors=T)

describe(GH_discretized%>%select(Precipitation_amount))
######################################################################################################################


####Applying the association rule mining method to get the decision rules

#####Convert DT to transactions, this helps in identifying how attributes/features are related to each other
GH_Table_Frame <- as.data.frame(Kariki_Reduct2) # Coerce Decision table back to data frame before coercing them to transactions
kariki_trans_GH<-as(GH_Table_Frame,"transactions")

itemLabels(kariki_trans_GH)
image(kariki_trans_GH)
head(kariki_trans_GH)

######Checking the frequencies of the items in the transaction##############################
itemFrequencyPlot(kariki_trans_GH,topN=20,type='absolute')
arules::itemFrequencyPlot(kariki_trans_GH,topN=20,
                          col = brewer.pal(8,'Pastel2'),
                          main='Relative Item Frequency Plot',
                          type="relative",
                          ylab="Item Frequency(Relative)")

#####In this plot we want to see the frequency of the items in the transaction, in this case the weather transaction from
####' from our kariki_trans_GH reduct, we see Rain with the factor No has the highest item set while High_temp has the lowest

# Checking for the most frequent itme based on the support
kariki_freq_tem_set <- eclat (kariki_trans_GH, parameter = list(supp = 0.02, maxlen = 15))
inspect(kariki_freq_tem_set)


##  Rule generation: I have to find an optimal support and confidence, use method 2 to generate good rules

###############################METHOD 1#######################################################################
###Generating the rule using Apriori method have to find an optimal support and confidence threshold to generate a significant number of rules
# and also generate non-redundant rules.
kariki_rules <- apriori(kariki_trans_GH,parameter = list(minlen = 2,supp= 0.01, conf = 0.8, maxlen=2),appearance = list(rhs= c("Rain=Yes", "Rain=No")))

kariki_rules <- sort(kariki_rules, by = "confidence", decreasing = FALSE)
#Inspecting rules with the highest confidence
arules::inspect(head(sort(kariki_rules, by='confidence'),37))

####### Prunning the rules to remove redundant rules#####
subset.k_rules<-is.subset(kariki_rules)
subset.k_rules

subset.k_rules[lower.tri(subset.k_rules, diag=T)] <- F
subset.k_rules

redundant<- apply(subset.k_rules,2,any)
redundant

rules.pruned <- kariki_rules[!redundant]
arules::inspect(rules.pruned)
####Turning the rules into a data-frame after being prunned we get 131 rules in total with the new data##############

rules_df <- data.frame(lhs=labels(lhs(rules.pruned)),rhs=labels(rhs(rules.pruned)),rules.pruned@quality)
rules_df


plot(rules.pruned, jitter = 0)
plot(rules.pruned, control = list(col=rainbow(7)),jitter = 0)

plot(rules.pruned, method="grouped")

plot(rules.pruned, method="graph")

plot(rules.pruned, method="paracoord", control=list(col=3,alpha=1, reorder=TRUE))

#### Looking at using LIFT instead of confidence and support to choose the best available rules####

inspect(head(sort(rules.pruned, by='lift'),10)) # Select the top most 10 rules
inspect(head(sort(rules.pruned, by= 'confidence'),50))

##########Writing the rules to an excel file
write.csv(rules_df,"D:\\Phd Research\\rules.csv", row.names = FALSE)






########################Converting the decision rules into new binary variables, I load the data set
binary_values_kariki <- read.csv("D:/Datasets/Kariki_binary_2_Nov.csv")
binary_values_kariki$RULE.INSTANCES<-NULL
RAGL <- glm(RAIN~., data = binary_values_kariki,family = "binomial",maxit = 100)
summary(RAGL)
with(summary(RAGL),1-deviance/null.deviance)### R-squared generates 1.000
anova(RAGL,test = 'Chisq')

library(MASS)

step_Rule <- stepAIC(D_R_GLM,trace = TRUE,direction = "both")

summary(step_Rule)


#####There is no need for dividing data set into training and testing, just do cross validation.

































################################ On the method using Lower and Upper Boundaries Rule Generation##########
#####Convert dec_table_1 to transactions, this helps in identifying how attributes/features are related to each other
Dec_table_Mat <- as.data.frame(dec_table_1) # Coerce Decision table back to data frame before coercing them to transactions
Dec_table_Trans<-as(GH_Table_Frame,"transactions")

itemLabels(Dec_table_Trans)
image(Dec_table_Trans)
head(Dec_table_Trans)

######Checking the frequencies of the items in the transaction##############################
itemFrequencyPlot(Dec_table_Trans,topN=20,type='absolute')
arules::itemFrequencyPlot(Dec_table_Trans,topN=20,
                          col = brewer.pal(8,'Pastel2'),
                          main='Relative Item Frequency Plot',
                          type="relative",
                          ylab="Item Frequency(Relative)")

#####In this plot we want to see the frequency of the items in the transaction, in this case the weather transaction from
####' from our Dec_table_Trans reduct, we see Rain with the factor No has the highest item set while High_temp has the lowest

# Checking for the most frequent items based on the support
kariki_Dec_tem_set <- eclat (Dec_table_Trans, parameter = list(supp = 0.275, maxlen = 15))
inspect(kariki_Dec_tem_set)


##  Rule generation: I have to find an optimal support and confidence, use method 2 to generate good rules

###############################METHOD 1#######################################################################
###Generating the rule using Apriori method have to find an optimal support and confidence threshold to generate a significant number of rules
# and also generate non-redundant rules.
kariki_rules_Dec <- apriori(Dec_table_Trans,parameter = list(minlen = 2,supp= 0.1, conf = 0.8, maxlen=15),appearance = list(rhs= c("Rain=Yes", "Rain=No")))

kariki_rules_conf_Dec <- sort(kariki_rules_Dec, by = "confidence", decreasing = FALSE)
inspect(head(kariki_rules_Dec))
#Inspecting rules with the highest confidence
inspect(head(sort(kariki_rules_Dec, by='confidence'),40))


####### Prunning the rules to remove redundant rules#####
subset.k_Dec<-is.subset(kariki_rules_Dec)
subset.k_Dec

subset.k_Dec[lower.tri(subset.k_Dec, diag=T)] <- F
subset.k_Dec

redundant_Dec<- apply(subset.k_Dec,2,any)
redundant_Dec

rules.pruned_dec <- kariki_rules_Dec[!redundant_Dec]
inspect(head(sort(rules.pruned_dec,by = 'confidence'), 10))
####Turning the rules into a data-frame

rules_df_dec <- data.frame(lhs=labels(lhs(rules.pruned_dec)),rhs=labels(rhs(rules.pruned_dec)),rules.pruned_dec@quality)
rules_df


plot(rules.pruned_dec)

plot(rules.pruned_dec, method="grouped")

plot(rules.pruned_dec, method="graph")


kariki_rules_yes <- arules::apriori(data = Dec_table_Trans,parameter = list(minlen = 2,supp= 0.02, conf = 0.5, maxlen=15),appearance = list(default = "lhs", rhs = c("Rain=Yes")))

inspect(head(sort(kariki_rules_yes,by = 'confidence'),40))

subset.k_Dec_yes<-is.subset(kariki_rules_yes)
subset.k_Dec_yes

subset.k_Dec_yes[lower.tri(subset.k_Dec_yes, diag=T)] <- F
subset.k_Dec_yes

redundant_Dec_yes<- apply(subset.k_Dec_yes,2,any)
redundant_Dec_yes

rules.pruned_dec_yes <- kariki_rules_yes[!redundant_Dec_yes]

rules_df_dec_yes <- data.frame(lhs=labels(lhs(rules.pruned_dec_yes)),rhs=labels(rhs(rules.pruned_dec_yes)),rules.pruned_dec_yes@quality)











######Rule generation 2 using confidence level of 1#####################NoT OPTIMAL #####################
kariki_rules_conf <- apriori(kariki_trans_GH,parameter = list(minlen = 2,supp= 0.02, conf = 1.0, maxlen=15),appearance = list(rhs= c("Rain=Yes", "Rain=No")))

kariki_rules_conf <- sort(kariki_rules_conf, by = "confidence", decreasing = FALSE)
inspect(head(kariki_rules_conf))
#Inspecting rules with the highest confidence
inspect(head(sort(kariki_rules_conf, by='confidence'),5))

####### Prunning the rules to remove redundant rules#####
subset.Conf_Rules<-is.subset(kariki_rules_conf)
subset.Conf_Rules

subset.Conf_Rules[lower.tri(subset.Conf_Rules, diag=T)] <- F
subset.Conf_Rules

redundant_conf<- apply(subset.Conf_Rules,2,any)
redundant_conf

prunned_rules <- kariki_rules[!redundant_conf]
inspect(prunned_rules)
####Turning the rules into a data-frame

rules_Conf <- data.frame(lhs=labels(lhs(prunned_rules)),rhs=labels(rhs(prunned_rules)),prunned_rules@quality)
rules_Conf

