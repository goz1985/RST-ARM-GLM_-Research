#.......................Experiment on the diabetes dataset......................
library(RoughSets)
library(RoughSetKnowledgeReduction)
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

#-----Loading the dataset-------------------------------------------------------
profvis({
  diabetes_df<-read.csv("D:/Datasets/Datasets for testing on model/diabetes.csv")
  str(diabetes_df)
  
  diabetes_df$Outcome<-factor(diabetes_df$Outcome)
  
  #.................Modelling on GLM function.....................................
  library(dplyr)
  library(Metrics)
  library(caret)
  library(caretEnsemble)
  
  set.seed(1)
  diabetes_partition<- createDataPartition(diabetes_df$Outcome, p =0.75, list = FALSE, times = 1)
  diabetes_train <- diabetes_df[diabetes_partition,]
  diabetes_test <- diabetes_df[-diabetes_partition,]
  
  diabetes_ctrl <- trainControl(method="repeatedcv", number=10, repeats=20,summaryFunction = twoClassSummary,
                                savePredictions=TRUE, classProbs=TRUE,preProc = c("center","scale"))
  
  #.............Modelling GLM model...............................................
  glm_diabetes<-glm(Outcome ~., data = diabetes_train,family = binomial,maxit=100)
  summary(glm_diabetes)
  anova(glm_diabetes,test = 'Chisq')
  diabetes_predict<-predict(glm_diabetes,diabetes_test,type="response")
  diabetes_cut_off<-ifelse(diabetes_predict>=0.5,1,0)
  diabetes_confusion_matrix<-confusionMatrix(as.factor(diabetes_test$Outcome),as.factor(diabetes_cut_off),positive = '1')
  diabetes_confusion_matrix
  AIC(glm_diabetes)
  BIC(glm_diabetes)
  
  #............Modelling decision tree............................................
  library(rpart)
  library(rpart.plot)
  diabetes_tree <-rpart(Outcome~.,data = diabetes_train,method="anova")
  summary(diabetes_tree)
  diabetes_tree_plot <-rpart.plot(diabetes_tree)
  
  
  
  #.............Modelling on GAM..................................................
  library(gam)
  diabetes_gam<-gam(Outcome ~., data = diabetes_train,family = binomial)
  summary(diabetes_gam)
  AIC(diabetes_gam)
  BIC(diabetes_gam)
  plot(diabetes_gam)
  
  
  
  #_________________________________________________________Rough-set Theory Feature detection_______________________________
  
  ###..................creating a decision table to be used for the roughset method...........................................
  
  diabetes_shuffled <- diabetes_df[sample(nrow(diabetes_df)),]
  diabetes_decision_table <- SF.asDecisionTable(diabetes_shuffled,decision.attr = 9, indx.nominal = c(9))
  
  #.....................Discretize the elements in the data sets for both training and testing................................
  
  diabetes.cut.values <- D.discretization.RST(diabetes_decision_table,type.method = "local.discernibility") 
  diabetes_discretized <- SF.applyDecTable(diabetes_decision_table,diabetes.cut.values)
  
  #...............Using greedy heuristci to reduce the attributes...............................................
  diabetes_GH_reduct <- FS.greedy.heuristic.reduct.RST(diabetes_discretized,qualityF = X.entropy,epsilon = 0.1)
  diabetes.Gh.Reduct <- SF.applyDecTable(diabetes_decision_table, diabetes_GH_reduct)
  str(diabetes.Gh.Reduct)
  
  write.csv(diabetes.Gh.Reduct,"D:\\Phd Research\\Experiment_2\\diabetes_GH.csv", row.names = FALSE)
  
  #...........................Association rule mining methods...................................................
  diabetes_dataframe <- as.data.frame(diabetes.Gh.Reduct) # Coerce Decision table back to data frame before coercing them to transactions
  diabetes_trans_GH<-as(diabetes_dataframe,"transactions")
  itemLabels(diabetes_trans_GH)
  image(diabetes_trans_GH)
  
  #...............................Generate decision rules using apriori method.....................................
  diabetes.rules <- apriori(diabetes_trans_GH,parameter = list(minlen = 2,supp= 0.01, conf = 0.75, maxlen=8),appearance = list(rhs= c("Outcome=1", "Outcome=0")))
  rules_diabetes_df <- data.frame(lhs=labels(lhs(diabetes.rules)),rhs=labels(rhs(diabetes.rules)),diabetes.rules@quality)
  
  diabetes.rules.sorted<- sort(diabetes.rules, by = "confidence", decreasing = TRUE)
  
  ## Prunning the rules of redundant or repeated rules#####
  
  diabetes.subset.rules<-is.subset(diabetes.rules.sorted)
  diabetes.subset.rules
  
  diabetes.subset.rules[lower.tri(diabetes.subset.rules, diag=T)] <- F
  diabetes.subset.rules
  
  redundant.diabetes<- apply(diabetes.subset.rules,2,any)
  redundant.diabetes
  
  diabetes.rules.pruned <- diabetes.rules.sorted[!redundant.diabetes]
  arules::inspect(diabetes.rules.pruned)
  
  diabetes.rules.df <- data.frame(lhs=labels(lhs(diabetes.rules.pruned)),rhs=labels(rhs(diabetes.rules.pruned)),diabetes.rules.pruned@quality)
  
  
  library(arules)
  library(arulesViz)
  
  mat.diabetes<-rules2matrix(diabetes.rules.pruned, measure = "support", reorder = "measure")
  mat.diabetes.df<-as.data.frame(mat.diabetes)
  view(mat.diabetes)
  
  library(Metrics)
  library(sjlabelled)
  library(sjPlot)
  library(flexmix) #Calculate BIC values
  library(lmtest)
  library(effects)#Interpreting the coefficients in our GLM
  library(gtsummary)
  
  diabetes_binary_values <- read.csv("D:/Phd Research/Experiment_2/diabetes_binary_values.csv")
  View(diabetes_binary_values)
  str(diabetes_binary_values)
  diabetes_binary_values$Outcome <- factor(diabetes_binary_values$Outcome, labels = c("No","Yes"))
  
  diabetes_binary_values$Rule.instance<-NULL
  diabetes_binary_model1 <- glm(Outcome~.,data = diabetes_binary_values,family = "binomial",maxit = 100)
  print(diabetes_binary_model1)
  tab_model(diabetes_binary_model1)
  tbl_regression(diabetes_binary_model1,exponentiate = TRUE)
  Anova(diabetes_binary_model1,test = 'Wald')
  anova(diabetes_binary_model1,test='Chisq')
  summary(diabetes_binary_model1)
  
  #..................Compare model performance between the models.....................................
  library(performance)
  print(compare_performance(diabetes_binary_model1,glm_diabetes,diabetes_gam))
})


