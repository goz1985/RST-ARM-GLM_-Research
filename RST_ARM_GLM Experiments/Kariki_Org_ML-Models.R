#####Using the model studio package to check on the explanantion for fitting a random forest model on the dataset
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

k_farm <- read_sheet("https://docs.google.com/spreadsheets/d/1y29ch-sv9UXSZUX9mRxqx6NleN6-XO3ifXDqkDzeOiE/edit#gid=0")

(n<-nrow(k_farm)) # Checking number of rows in the data which is 1619 observations with 17 varibales
c(as.character(k_farm$Date[1]), as.character(k_farm$Date[n])) # the date range from 2/3/2018 to 18/8/2022
head(k_farm$Rain)
k_farm$Rain <- factor(k_farm$Rain) # Rain Yes/No to factor    ordered = TRUE
k_farm$Date <- as.Date(k_farm$Date, '%m/%d/%Y') # Date column to date
str(k_farm)

view(k_farm)
(cols_withNa <- apply(k_farm, 2, function(x) sum(is.na(x))))

kariki_farm3 <- k_farm[complete.cases(k_farm),]
str(kariki_farm3)
(cols_withNa <- apply(kariki_farm3, 2, function(x) sum(is.na(x))))
kariki_farm3$Date <- NULL # removing the date column
kariki_farm3$Windspeed_low <- NULL
kariki_farm3$Precipitation_amount<-NULL
kariki_farm3$Rain<-as.factor(kariki_farm3$Rain)
kariki_farm3$Rain<-as.character(kariki_farm3$Rain)
kariki_farm3$Rain[kariki_farm3$Rain=='0']<-'No'
kariki_farm3$Rain[kariki_farm3$Rain=='1']<-'Yes'
kariki_farm3$Rain<-factor(kariki_farm3$Rain, levels =  c("No","Yes"))
str(kariki_farm3)

set.seed(123)
kariki_train3 <- createDataPartition(kariki_farm3$Rain, p =0.75, list = FALSE)
training3 <- kariki_farm3[kariki_train3,]
testing3 <- kariki_farm3[-kariki_train3,]
control3 <- trainControl(method="repeatedcv", number=10, repeats=3,
                         savePredictions=TRUE, classProbs=TRUE,preProc = c("center","scale"))


#######################Fit a random forest model##############################################################
# Create a Random Forest model with default parameters
system.time(fit.rf <- train(Rain~., data=training3, method="rf", metric="Accuracy", trControl=control3,nbagg = 200))
predictions_rf3<-predict(object=fit.rf ,testing3, type="raw")
table(predictions_rf3)
confusionMatrix(predictions_rf3,testing3$Rain)
summary(fit.rf)
fit.rf_Imp <- varImp(fit.rf)
plot(fit.rf_Imp)

############################# Creating a parallel socket cluster################################

# Create a parallel socket cluster
cl <- makeCluster(8) # use 8 workers
registerDoParallel(cl) # register the parallel backend

# Fit trees in parallel and compute predictions on the test set
predictions <- foreach(
  icount(160), 
  .packages = "rpart", 
  .combine = cbind
) %dopar% {
  # bootstrap copy of training data
  index <- sample(nrow(training3), replace = TRUE)
  kfarm_train_boot <- training3[index, ]  
  
  # fit tree to bootstrap copy
  bagged_tree <- rpart(
    Rain ~ ., 
    control = rpart.control(minsplit = 2, cp = 0),
    data = kfarm_train_boot
  ) 
  
  predict(bagged_tree, newdata = testing3)
}

predictions[1:5, 1:7]

###################################################### Giving an error############Check on it

predictions %>%
  as.data.frame() %>%
  mutate(
    observation = 1:n(),
    actual = testing3$Rain) %>%
  tidyr::gather(tree, predicted, -c(observation, actual)) %>%
  group_by(observation) %>%
  mutate(tree = stringr::str_extract(tree, '\\d+') %>% as.numeric()) %>%
  ungroup() %>%
  arrange(observation, tree) %>%
  group_by(observation) %>%
  mutate(avg_prediction = cummean(predicted)) %>%
  group_by(tree) %>%
  summarize(RMSE = RMSE(avg_prediction, actual)) %>%
  ggplot(aes(tree, RMSE)) +
  geom_line() +
  xlab('Number of trees')


###########################Boosting the Original data###############################################
library(gbm)
set.seed(123)
fitControl = trainControl(method="cv", number=5, returnResamp = "all")
GBM_model = train(Rain~., data=training3, method="gbm",distribution="bernoulli", trControl=fitControl, verbose=F)
confusionMatrix(GBM_model)
mPred = predict(GBM_model, testing3, na.action = na.pass)
postResample(mPred, testing3$Rain)
confusionMatrix(mPred, testing3$Rain)
getTrainPerf(GBM_model)

mResults = predict(GBM_model, testing3, na.action = na.pass, type = "prob")
mResults$obs = testing3$Rain
head(mResults)

mnLogLoss(mResults, lev = levels(mResults$obs))

mResults$pred = predict(GBM_model,testing3, na.action = na.pass)
multiClassSummary(mResults, lev = levels(mResults$obs))


###################Employing the GLM model####################################################
#####Using the data in ML_Models_ensemble_kariki_surface######################################
########I updated the data with weather data from 5/11/2022 to 15/11/2022..Had some really significant change#####

Model_GLM_ORG<-glm(Rain~., data = training,family = "binomial",maxit = 100)
summary(Model_GLM_ORG)
predict(Model_GLM_ORG,data=testing,type = 'terms')
with(summary(Model_GLM_ORG),1-deviance/null.deviance)#calculating R-squared
plot_model(Model_GLM_ORG,vline.color = "green",sort.est = TRUE,show.values = TRUE,type = "pred")#Gives probability curves of the features against the target variable
Model_GLM_ORG$effects
anova(Model_GLM_ORG)

