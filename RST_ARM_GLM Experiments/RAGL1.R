################### In this file the work will be to model the deduced binary values on the GLM model#######################################################################################
#' The work done here is the continuation of the work from the file ARM_RST.r file where I did the following:
#' 1. I loaded the data from the online google sheet which I had captured from the https://www.wunderground.com/dashboard/pws/ITHIKA2
#' 2. Preprocessed the data and removed all null values.
#' 3. Interaction detection via Rough Set theory where I used the indiscernibility relation and greedy heuristic reduct generation method to select the important features for prediction.
#' 4. Generated association rules and selected the best 30 rules with a high confidence of above 80%. Ensured the removal of redundant and duplicate rules.
#' 5. Formulated binary rules from step 4 above and saved them in a new data frame
#' 6. Modeled these binary values on the GLM model.
#' 
#'This work here is for modelling these vlaues on the GLM model and then getting the experiment results.


################################# The Modelling and reviewing of the test experiments ######################################################################################################

library(caret)
library(caretEnsemble)
library(googlesheets4)
library(Metrics)
library(sjlabelled)
library(sjPlot)
library(flexmix) #Calculate BIC values
library(lmtest)
library(effects)#Interpreting the coefficients in our GLM

kariki_binary_values <- read.csv("D:/Binary_values.csv")
View(kariki_binary_values)

kariki_binary_values$RULE.INSTANCES<-NULL

#### Fitting the GLM model and assessing it's viability ############
kariki_binary_model1 <- glm(Rain~.,data = kariki_binary_values,family = "binomial",maxit = 100)
summary(kariki_binary_model1)
BIC(kariki)
with(summary(kariki_binary_model1),1-deviance/null.deviance)####Simple method to get R-squared

#################################R-sqaured model#############################################
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
########################################################################################################

predict(kariki_binary_model1,type = 'response')
anova(kariki_binary_model1, test = 'LRT')
mae(kariki_binary_values$Rain,predict(kariki_binary_model1))
rmse(kariki_binary_values$Rain,predict(kariki_binary_model1))


coef(kariki_binary_model1)
exp(coef(kariki_binary_model1))
lrtest(kariki_binary_model1)
##
#'When running the anova test we find that Low_Temp[ 14.5,15.9]; Dewpoint_High[ 15.8,22.4]; Dewpoint_low[ 8.4,10.9];and Humidity_High..90.99.
#' are important predictors when it comes to Rain. High humidity has a high impact when it comes to Rain.
#' We also note the AIC value has droped till 50

c(extractAIC(kariki_original_model1), extractAIC(kariki_binary_model1)) # Shows the model with the binary values is prefered for use.
plot_model(kariki_binary_model1,vline.color = "red",sort.est = TRUE,show.values = TRUE,type = "pred")
plot_model(kariki_binary_model1,vline.color = "red",sort.est = TRUE,show.values = TRUE)# This gives a better explanantion of the predictors to consider while looking into rain aspect.




#####Fitting a GLM model on the kariki_farm data without RST and ARM detection########
#' I did not convert the Rain factor into Character factor of Yes/No
k_farm <- read_sheet("https://docs.google.com/spreadsheets/d/1y29ch-sv9UXSZUX9mRxqx6NleN6-XO3ifXDqkDzeOiE/edit#gid=0")

(n<-nrow(k_farm)) # Checking number of rows in the data which is 1619 observations with 17 varibales
c(as.character(k_farm$Date[1]), as.character(k_farm$Date[n])) # the date range from 2/3/2018 to 18/8/2022
head(k_farm$Rain)
k_farm$Rain <- factor(k_farm$Rain) # Rain Yes/No to factor    ordered = TRUE
k_farm$Date <- as.Date(k_farm$Date, '%m/%d/%Y') # Date column to date
str(k_farm)

view(k_farm)
(cols_withNa <- apply(k_farm, 2, function(x) sum(is.na(x))))

kariki_farm2 <- k_farm[complete.cases(k_farm),]
str(kariki_farm2)
(cols_withNa <- apply(kariki_farm2, 2, function(x) sum(is.na(x))))
kariki_farm2$Date <- NULL # removing the date column
kariki_farm2$Windspeed_low <- NULL
kariki_farm2$Precipitation_amount<-NULL

kariki_farm2$Rain<-as.character(kariki_farm2$Rain)
kariki_farm2$Rain[kariki_farm2$Rain=='0']<-'No'
kariki_farm2$Rain[kariki_farm2$Rain=='1']<-'Yes'
kariki_farm2$Rain<-factor(kariki_farm2$Rain, levels =  c("No","Yes"))
str(kariki_farm2)


set.seed(123)
kariki_train2 <- createDataPartition(kariki_farm2$Rain, p =0.75, list = FALSE)
training2 <- kariki_farm2[kariki_train2,]
testing2 <- kariki_farm2[-kariki_train2,]

control2 <- trainControl(method="repeatedcv", number=10, repeats=3,
                        savePredictions=TRUE, classProbs=TRUE,preProc = c("center","scale"))

model2<-train(Rain~.,data = training2,method = 'glm',trControl=control2,metric='Accuracy',maxit=100)
getTrainPerf(model2)
summary(model2)
kariki_predict<-predict(model2,testing2)
confusionMatrix(kariki_predict,testing2$Rain)
summary(model2)
coef(model2)
MAE(testing2$Rain,predict(model2,testing2))




##############################Using the GLM method in R instead of the caret Package####################
kariki_original_model1<-glm(Rain~., data = training2,family = "binomial",maxit = 100)

summary(kariki_original_model1)
resid(kariki_original_model1,type = 'pearson')
anova(kariki_original_model1,test =c("LR"))

confint(kariki_original_model1)# gives 95% confidence interval of coefficients
exp(confint(kariki_original_model1))# gives conf intervals of odds ratios
exp(coef(kariki_original_model1))# gives odds ratios
exp(cbind(OddRatiomodeln=coef(kariki_original_model1),confint(kariki_original_model1)))# odds & confidence interval at once


predict(kariki_original_model1,data=testing2,type = 'terms')
mae(testing2$Rain,predict(kariki_original_model1))
rmse(testing2$Rain,predict(kariki_original_model1))



plot_model(kariki_original_model1,vline.color = "green",sort.est = TRUE,show.values = TRUE,type = "pred")#Gives probability curves of the features against the target variable

anova(kariki_original_model1,kariki_binary_model1)

##### Function to calculate R squared for the model#####
model2Rsq<-function(kariki_original_model1)
{
  dev = kariki_original_model1$deviance
  nullDev = kariki_original_model1$null.deviance
  modelN = length(kariki_original_model1$fitted.values)
  R.1 <- 1 - dev / nullDev
  R.cs <- 1- exp(-(nullDev-dev)/modelN)
  R.n <- R.cs /(1-(exp(-(nullDev / modelN))))
  cat("Pseudo R-sq for logistic regression\n")
  cat("Hosmer & Lemeshow R-sq ", round(R.1, 4),"\n")
  cat("Cox and Snell R-sq ", round(R.cs, 4), "\n")
  cat("Nagelkerke R-sq ", round(R.n, 4), "\n")
}

model2Rsq(kariki_original_model1) # R-Squared value for the GLM fit on data without interaction detection


####Function 2 to calculate R-squared##############################

with(summary(kariki_original_model1),1-deviance/null.deviance)### A simpler way of calculating R-squared


###########################Using the GLMNET package because the other glm packages are having fitting issues###################
library(glmnet)
model_glmnet<-train(Rain~.,data = training2,method='glmnet',trControl = control2)
plot(model_glmnet)
predict(model_glmnet,testing2$Rain)
predict(model_glmnet,testing2)
predict(model_glmnet,testing2,offset = offset)
print(model_glmnet)

