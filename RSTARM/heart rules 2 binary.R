# Trying out the conversion
#.......................Experiment on the diabetes dataset......................
library(RoughSets)
#library(RoughSetKnowledgeReduction)
library(googlesheets4)
library(arules)
library(dplyr)
library(RColorBrewer)
library(arulesViz)
library(tidyverse)
library(tidymodels)
library(ranger)
library(randomForest)
library(skimr)
library(ggplot2)
library(pROC)
library(plyr)
library(purrr)
library(car)
library(profvis)
#-----Loading the dataset-------------------------------------------------------
profvis({
  heart_df<-read.csv("D:/Datasets/Datasets for testing on model/heart.csv")
  str(heart_df)
  summary(heart_df)
  
  #...changing some of the attributes to factor...................................
  heart_df$sex <- as.factor(heart_df$sex)
  heart_df$cp <- as.factor(heart_df$cp)
  heart_df$restecg <- as.factor(heart_df$restecg)
  heart_df$exang <- as.factor(heart_df$exang)
  heart_df$thal <- as.factor(heart_df$thal)
  heart_df$target <- as.factor(heart_df$target)
  
  heart_df$fbs<-as.factor(heart_df$fbs)
  heart_df$slope<-as.factor(heart_df$slope)
  heart_df$ca<-as.factor(heart_df$ca)
  
  #_________________________________________________________Rough-set Theory Feature detection_______________________________
  #' check on this as it is generating an error while generating the reduct while using the greedy heuristic but 
  #' Generates while using the Quick Reduct method.
  ###..................creating a decision table to be used for the rough-set method...........................................
  
  heart_shuffled <- heart_df[sample(nrow(heart_df)),]
  heart_decision_table <- SF.asDecisionTable(heart_shuffled,decision.attr = 14, indx.nominal = c(2,3,6,7,9,11,12,13,14))
  
  #.....................Discretize the elements in the data sets for both training and testing................................
  
  heart.cut.values <- D.discretization.RST(heart_decision_table,type.method = "local.discernibility") 
  heart_discretized <- SF.applyDecTable(heart_decision_table,heart.cut.values)
  
  #...............Using greedy heuristci to reduce the attributes...............................................
  heart_GH_reduct <- FS.greedy.heuristic.reduct.RST(heart_discretized,qualityF = X.entropy,epsilon = 0.0)
  heart.Gh.Reduct <- SF.applyDecTable(heart_decision_table, heart_GH_reduct)
  str(heart.Gh.Reduct)
  
  #...............Association rule mining process.............................................................
  heart_dataframe <- as.data.frame(heart.Gh.Reduct) # Coerce Decision table back to data frame before coercing them to transactions
  heart_trans_GH<-as(heart_dataframe,"transactions")
  itemLabels(heart_trans_GH)
  
  heart.rules <- apriori(heart_trans_GH,parameter = list(minlen = 4,supp= 0.01, conf = 0.8, maxlen=10),appearance = list(rhs= c("target=1", "target=0")))
  
  heart.rules.sorted<- sort(heart.rules, by = "confidence", decreasing = TRUE)
  
  ## Prunning the rules of redundant or repeated rules#####
  
  heart.subset.rules<-is.subset(heart.rules.sorted)
  heart.subset.rules
  
  heart.subset.rules[lower.tri(heart.subset.rules, diag=T)] <- F
  heart.subset.rules
  
  redundant.heart<- apply(heart.subset.rules,2,any)
  redundant.heart
  
  heart.rules.pruned <- heart.rules.sorted[!redundant.heart]
  arules::inspect(heart.rules.pruned)
  
  # Converting the rules to binary values
  items <- unique(unlist(lapply(1:length(heart.rules.pruned), function(i) c(lhs = labels(lhs(heart.rules.pruned[i])), rhs = labels(rhs(heart.rules.pruned[i]))))))
  
  # Create a binary matrix
  binaryMatrix <- matrix(0, nrow = length(heart.rules.pruned), ncol = length(items), dimnames = list(NULL, items))
  
  # Fill matrix with 1s where items are in rules
  for (i in seq_len(length(heart.rules.pruned))) {
    rule_items <- c(lhs = labels(lhs(heart.rules.pruned[i])), rhs = labels(rhs(heart.rules.pruned[i])))
    binaryMatrix[i, rule_items] <- 1
  }
  
  # Convert to data frame
  heart_binary_df <- as.data.frame(binaryMatrix)
  
  set.seed(123)
  
  # Split the data into training and test sets
  train_indices <- sample(1:nrow(heart_binary_df), 0.7 * nrow(heart_binary_df))
  train_set <- heart_binary_df[train_indices, ]
  test_set <- heart_binary_df[-train_indices, ]
  
  # Fit the model to the training data
  model <- glm(data = train_set, family = binomial)
})