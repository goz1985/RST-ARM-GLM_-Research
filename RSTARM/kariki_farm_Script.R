#____________Clean script for RAGL on KAriki_Farm Data___________________________________
#__________Step 1, load dataset from google sheets, preprocess it, model it on glm, decision tree and gam models________
library(RoughSets)
library(googlesheets4)
library(arules)
library(dplyr)
library(RColorBrewer)
library(arulesViz)
library(mosaic)

profvis({
  gs4_deauth()
  k_link<-"https://docs.google.com/spreadsheets/d/1y29ch-sv9UXSZUX9mRxqx6NleN6-XO3ifXDqkDzeOiE/edit?usp=sharing"
  k_farm<-read_sheet(k_link)
  
  
  (n<-nrow(k_farm)) # Checking number of rows in the data which is 1619 observations with 17 varibales
  c(as.character(k_farm$Date[1]), as.character(k_farm$Date[n])) # the date range from 2/3/2018 to 18/8/2022
  head(k_farm$Rain)
  k_farm$Rain <- factor(k_farm$Rain) # Rain Yes/No to factor    ordered = TRUE
  k_farm$Date <- as.Date(k_farm$Date, '%m/%d/%Y') # Date column to date
  str(k_farm)
  
  #view(k_farm)
  (cols_withNa <- apply(k_farm, 2, function(x) sum(is.na(x))))# Getting missing value tabulation
  
  #--------------------------- Picking only complete values, I create another data frame kariki_farm2################I removed missing value rows in the dataset
  kariki_farm2 <- k_farm[complete.cases(k_farm),]
  str(kariki_farm2)
  (cols_withNa <- apply(kariki_farm2, 2, function(x) sum(is.na(x))))
  kariki_farm2$Date <- NULL # removing the date column
  kariki_farm2$Windspeed_low <- NULL # removing windspeed low as it has no effect on the prediction 
  kariki_farm2$Rain <- factor(kariki_farm2$Rain, labels = c("No","Yes"))
  str(kariki_farm2)
  #-----------------------------Removing the precipitation amount because it will influence the outcome variable#########
  kariki_farm2$Precipitation_amount<- NULL
  
  ##------------------------------------Splitting into training and testing sets, then carrying out a cross validation on the same####################################################################
  library(dplyr)
  library(Metrics)
  library(caret)
  library(caretEnsemble)
  set.seed(123)
  kariki_train <- createDataPartition(kariki_farm2$Rain, p =0.75, list = FALSE)
  training <- kariki_farm2[kariki_train,]
  testing <- kariki_farm2[-kariki_train,]
  
#_____________Testing on the GLM, decision tree and GAM models________________________________________________
  kariki_GLM1 <- glm(Rain~.,data = training,family = "binomial",maxit = 100)
  summary(kariki_GLM1)
  anova(kariki_GLM1,test = 'Chisq')
  kariki_predict<-predict(kariki_GLM1,testing,type="response")
  kariki_cut_off<-ifelse(kariki_predict>=0.5,1,0)
  AIC(kariki_GLM1)
  BIC(kariki_GLM1)
  
  
  #---------------------------Applying The GLM model on the data before using RST to get the interaction terms from the data set..84% accuracy##########
  control <- trainControl(method="repeatedcv", number=10, repeats=20,summaryFunction = twoClassSummary,
                          savePredictions=TRUE, classProbs=TRUE,preProc = c("center","scale"))
  
  predictor_rain <-c("High_Temp","Avg_Temp","Low_Temp","Dewpoint_High","Dewpoint_Avg","Dewpoint_low","Humidity_High","Humidity_Avg","Humidity_Low","Windspeed_High","Windspeed_Avg")
  prediction_formula <- as.formula(paste("Rain", paste(predictor_rain, collapse="+"), sep="~"))
  kariki_ML <- train(prediction_formula,data = training,method = "glm",family="binomial", trControl = control, metric = 'Accuracy',maxit = 100)
  kariki_ML$results
  kariki_ML$finalModel
  summary(kariki_ML)
  kariki_ML$metric
  exp(coef(kariki_ML$finalModel))
  #confint(kariki_ML)
  
  printCoefmat( coef( summary(kariki_ML) ) ) #Wald test for single regression coefficients
  glm_Org <- predict(kariki_ML,testing,type = "raw")
  table(glm_Org)
  confusionMatrix(glm_Org,testing$Rain)
  
  
  #______________________Modelling GAM___________________________________________
  library(gam)
  kariki_gam<-gam(Rain~., data = training,family = binomial)
  summary(kariki_gam)
  AIC(kariki_gam)
  BIC(kariki_gam)
  anova(kariki_GLM1,kariki_gam,test="Chisq")
  
  #_____________Applying the decision tree model___________________________________________________________
  library(rpart)
  library(rpart.plot)
  kariki_tree <-rpart(Rain ~., data = training, method="anova")
  summary(kariki_tree)
  
  kariki_tree_plot <-rpart.plot(kariki_tree)
  kariki_tree_predict<-predict(kariki_tree,data = testing,type="vector")
  confusionMatrix(kariki_tree_predict,testing$Rain,positive = '1')
  
  
  
  #____________Step 2(a) Featyre interaction detection begining with selecting important features with Rough Set theory_______
  kariki_shuffled <- kariki_farm2[sample(nrow(kariki_farm2)),]
  kariki_table <- SF.asDecisionTable(kariki_shuffled,decision.attr = 14, indx.nominal = c(14))
  
  
  #___________Discretize the elements in the data sets for both training and testing_________
  cut.values <- D.discretization.RST(kariki_table,type.method = "global.discernibility")  
  kariki_discretized <- SF.applyDecTable(kariki_table,cut.values)
  
  #_______________Feature selection using Greedy Heuristic Method where it computes the approximate reduct_____________________
  GH_2_RST <- FS.greedy.heuristic.reduct.RST(kariki_discretized,qualityF = X.entropy,epsilon = 0.1)
  Kariki_Reduct2 <- SF.applyDecTable(kariki_table, GH_2_RST)
  
  
  
  #______________Step 2(b)Applying the association rule mining method to get the decision rules____________
  
  #_______________________________Convert DT to transactions, this helps in identifying how attributes/features are related to each other________________________
  GH_Table_Frame <- as.data.frame(Kariki_Reduct2) # Coerce Decision table back to data frame before coercing them to transactions
  kariki_trans_GH<-as(GH_Table_Frame,"transactions")
  
  itemLabels(kariki_trans_GH)
  image(kariki_trans_GH)
  head(kariki_trans_GH)
  
  #----------------Checking the frequencies of the items in the transaction---------------------------
  itemFrequencyPlot(kariki_trans_GH,topN=20,type='absolute')
  arules::itemFrequencyPlot(kariki_trans_GH,topN=20,
                            col = brewer.pal(8,'Pastel2'),
                            main='Relative Item Frequency Plot',
                            type="relative",
                            ylab="Item Frequency(Relative)")
  
  #----------------------In this plot we want to see the frequency of the items in the transaction, in this case the weather transaction from
  #-------------------------- from our kariki_trans_GH reduct, we see Rain with the factor No has the highest item set while High_temp has the lowest
  
  #---------------Checking for the most frequent itme based on the support
  kariki_freq_tem_set <- eclat (kariki_trans_GH, parameter = list(supp = 0.02, maxlen = 15))
  #inspect(kariki_freq_tem_set)
  
  #---------------Generating the rule using Apriori method have to find an optimal support and confidence threshold to generate a significant number of rules
  #-----------and also generate non-redundant rules.
  kariki_rules <- apriori(kariki_trans_GH,parameter = list(minlen = 2,supp= 0.01, conf = 0.8, maxlen=4),appearance = list(rhs= c("Rain=Yes", "Rain=No")))
  
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
  #------Turning the rules into a data-frame after being prunned we get 131 rules in total with the new data---------------------
  
  #rules_df <- data.frame(lhs=labels(lhs(rules.pruned)),rhs=labels(rhs(rules.pruned)),rules.pruned@quality)
  #rules_df
  
  
  plot(rules.pruned, jitter = 0)
  plot(rules.pruned, control = list(col=rainbow(7)),jitter = 0)
  
  plot(rules.pruned, method="grouped")
  
  plot(rules.pruned, method="graph")
  
  #plot(rules.pruned, method="paracoord", control=list(col=3,alpha=1, reorder=TRUE))
  
  # Converting the rules to binary values
  items <- unique(unlist(lapply(1:length(rules.pruned), function(i) c(lhs = labels(lhs(rules.pruned[i])), rhs = labels(rhs(rules.pruned[i]))))))
  
  # Create a binary matrix
  kariki_binaryMatrix <- matrix(0, nrow = length(rules.pruned), ncol = length(items), dimnames = list(NULL, items))
  
  # Fill matrix with 1s where items are in rules
  for (i in seq_len(length(rules.pruned))) {
    rule_items <- c(lhs = labels(lhs(rules.pruned[i])), rhs = labels(rhs(rules.pruned[i])))
    kariki_binaryMatrix[i, rule_items] <- 1
  }
  
  # Convert to data frame
  kariki_binary_2_Nov <- as.data.frame(kariki_binaryMatrix)
  
  
  library(caret)
  library(caretEnsemble)
  library(googlesheets4)
  library(Metrics)
  library(sjlabelled)
  library(sjPlot)
  library(flexmix) #Calculate BIC values
  library(lmtest)
  library(effects)#Interpreting the coefficients in our GLM
  library(gtsummary)
  library(performance)
  kariki_binary_values <- read.csv("D:/Datasets/Kariki_binary_2_Nov.csv")
  View(kariki_binary_values)
  str(kariki_binary_values)
  kariki_binary_values$RAIN <- factor(kariki_binary_values$RAIN, labels = c("No","Yes"))
  
  kariki_binary_values$RULE.INSTANCES<-NULL
  
  #----------------------------------Fitting the GLM model and assessing it's viability---------------------
  kariki_binary_model1 <- glm(RAIN~.,data = kariki_binary_values,family = "binomial",maxit = 100)
  tab_model(kariki_binary_model1) # Summary of regression model as a table
  tbl_regression(kariki_binary_model1,exponentiate = TRUE)
  summary(kariki_binary_model1)$coefficients
  BIC(kariki_binary_model1)
  with(summary(kariki_binary_model1),1-deviance/null.deviance)####Simple method to get R-squared
  kariki_binary_model1$null.deviance
  kariki_binary_model1$df.residual
  kariki_binary_model1$df.null
  
  step_kariki_Binary <- step(kariki_binary_model1,direction = "both")
  
  #---------------------------------R-squared model---------------------------------------------------
  RAGL<-function(kariki_binary_model1)
  {
    dev = kariki_binary_model1$deviance
    nullDev = kariki_binary_model1$null.deviance
    modelN = length(kariki_binary_model1$fitted.values)
    R.1 <- 1 - dev / nullDev
    R.cs <- 1- exp(-(nullDev-dev)/modelN)
    R.n <- R.cs /(1-(exp(-(nullDev / modelN))))
    cat("Pseudo R-sq for logistic regression\n")
    cat("Hosmer & Lemeshow R-sq ", round(R.1, 4),"\n")
    cat("Cox and Snell R-sq ", round(R.cs, 4), "\n")
    cat("Nagelkerke R-sq ", round(R.n, 4), "\n")
  }
  
  RAGL(kariki_binary_model1)
  
  #--------------------############-----------------------------------------------------
  
  print(compare_performance(kariki_binary_model1,kariki_GLM1,kariki_gam,kariki_tree))
  
  
})