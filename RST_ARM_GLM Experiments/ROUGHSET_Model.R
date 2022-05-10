# Dealing with KAriki farm using the rough set model and formulation of decision rules
# In this notebook, I will be using roughset theory on the kariki dataset to reduce the set of terms.
# Also I will want to see whether having a discretized dataset is better than having the original data.
# Which discretization methods are better

# The notebook will start by loading neccessary libraries, loading data, preprocessing it and removing null values
# Will also want to see whether I can do imputation on the data

library(RoughSets)
library(RoughSetKnowledgeReduction)

#Loading the dataset and preprocessing it
kariki_farm <- read.csv("D:/datasets/Kariki_Farm1.csv", stringsAsFactors = FALSE, fileEncoding="latin1")
(n<-nrow(kariki_farm)) # Checking number of rows in the data which is 1179
c(as.character(kariki_farm$Date[1]), as.character(kariki_farm$Date[n])) # the date range from 2/3/2018 to 20/5/2021
head(kariki_farm$Rain.Yes.No.)
kariki_farm$Rain <- factor(kariki_farm$Rain, labels = c("No","Yes"))# Rain Yes/No to factor
kariki_farm$Date <- as.Date(kariki_farm$Date, '%m/%d/%Y') # Date column to date
str(kariki_farm)# When looking at the high and low numeric pressure, I had to convert them to numeric because they were being considered as being factors yet theyr weren't

# Will have to check on the aspect of changing this to numeric from factor because it is giving weird values
kariki_farm$High.Hpa. <- as.numeric(kariki_farm$High.Hpa.)
kariki_farm$Low.Hpa. <- as.numeric(kariki_farm$Low.Hpa.)
str(kariki_farm)
view(kariki_farm)
(cols_withNa <- apply(kariki_farm, 2, function(x) sum(is.na(x))))

# Value imputation 
summary(kariki_farm) # done to get the overlay of the data.
# Creating a visual representation of the missing values
library(VIM)
Kariki_plot <- aggr(kariki_farm, col=c('navyblue','yellow'),numbers = TRUE, sortVars = TRUE, labels =names(kariki_farm),cex.axis=.7,gap=3, ylab=c("Missing data","Pattern"))

#Imputing the missing values using a suitable method. Still referring 

# Will use mice as the first method, then will try using HMsic (with the aregimpute()method)
library(mice)
library(missForest)
library(Hmisc) #Giving an error message while loading

# Imputation with MICE package
kariki_imputed <- mice(kariki_farm, m =1, method = "pmm", maxit = 50, seed = 123)
kariki_imputed_complete <- complete(kariki_imputed)

#Impute with the HMISC package...This package not loading

# If not imputing will be removing the rows with missing data
(cols_withNa <- apply(kariki_farm, 2, function(x) sum(is.na(x))))
kariki_farm2 <- kariki_farm[complete.cases(kariki_farm),]
str(kariki_farm2)
(cols_withNa <- apply(kariki_farm2, 2, function(x) sum(is.na(x))))
kariki_farm2$Date <- NULL # removing the date column
#Remove windspeed as it doesnt add any value to our research
kariki_farm2$Windspeed_low <-NULL

view(kariki_farm2)

##### Dealing with the roughset
library(arules)
library(dplyr)

# When dealing with RST technology:
#' 1. Create a decision table from the given dataframe
#' 2. Discretize the values which may be numeric into nominal values using the RST methods of global, local and unsupervised
#' 3. Feature selection

kariki_shuffled <- kariki_farm2[sample(nrow(kariki_farm2)),]
kariki_DT <- SF.asDecisionTable(kariki_shuffled, decision.attr = 15, indx.nominal = c(1:14)) # Indx.nominal is picking conditional attributes
####kariki_DT_cutValues <- D.discretization.RST(kariki_DT, type.method = "local.discernibility")## Saying all conditional attributes are nominal

# Computing the indiscernibility relation
IND <- BC.IND.relation.RST(kariki_DT)

# Compute Upper and Lower Approximations
kariki_roughset <- BC.LU.approximation.RST(kariki_DT,IND)

# Determining the regions
kariki.regions <- BC.positive.reg.RST(kariki_DT,kariki_roughset)

# Discernibility matrix formulation
kariki.matrix <- BC.discernibility.mat.RST(kariki_DT)

# Reduct formulation using
#' 1 Greedy heuristic
GH_Kariki_reduct <- FS.greedy.heuristic.reduct.RST(kariki_DT,qualityF = X.entropy,epsilon = 0.0)
GH_Table <- SF.applyDecTable(kariki_DT,GH_Kariki_reduct)
# With this method we get a reduct made up of 4 variables: High_temp, Dew_point, Precipitation amount and Rain

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
set.seed(123)
GH_kariki_train <- createDataPartition(GH_Table$Rain, p =0.75, list = FALSE)
GH_training <- GH_Table[GH_kariki_train,]
GH_testing <- GH_Table[-GH_kariki_train,]

#### Ensemble methods#########
GH_control <- trainControl(method="repeatedcv", number=10, repeats=3,
                        savePredictions=TRUE, classProbs=TRUE,preProc = c("center","scale"))

## TreeBag Method, and running the testing data on the developed model####
seed <- 123
metric <- "Accuracy"
set.seed(seed)
fit.treebag_GH <- train(Rain~., data=GH_training, method="treebag", metric=metric, trControl=GH_control)

# Running the predictions..the model on the reduct gives an accuracy of 89%
predictions_treebag_GH<-predict(object=fit.treebag_GH ,GH_testing, type="raw")
table(predictions_treebag_GH)
confusionMatrix(predictions_treebag_GH,GH_testing$Rain)

### Running on the random forest model#### Was taking to much time to run
set.seed(seed)
fit.rf_GH <- train(Rain~., data=GH_training, method="rf", metric=metric, trControl=GH_control)

predictions_rf_GH<-predict(object=fit.rf_GH ,GH_testing, type="raw")
table(predictions_rf_GH)
confusionMatrix(predictions_rf_GH,GH_testing$Rain)



####Applying the association rule mining method to get the decision rules


#Convert DT to transactions, this helps in identifying how attributes/features are related to each other
GH_Table_Frame <- as.data.frame(GH_Table) # Coerce Decision table back to dataframe before coercing them to transactions
kariki_trans_GH<-as(GH_Table_Frame,"transactions")

itemLabels(kariki_trans_GH)
image(kariki_trans_GH)


#Generating the rule susing apriori method, will generate only two rules, need to fine tune in step 149 to get more rules
kariki_rules <- apriori(kariki_trans_GH)

#Inspecting rules with the highest confidence
inspect(head(sort(kariki_rules, by='confidence'),5))


# Fine tunning the above function for deducing rules by tunning the support and confidence
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






#' 2. Super reduct method
# Using the discernibility matrix kariki.matrix to formulate a reduct using all reduct computation

reduct <- FS.all.reducts.computation(kariki.matrix) # Method very time consuming..check system time computation
#The method henerates 477 reducts..generally not a feasible method as the research i proposed the greedy heuristic method.





