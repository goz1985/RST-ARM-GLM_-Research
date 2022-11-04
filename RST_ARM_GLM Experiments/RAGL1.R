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

kariki_binary_values <- read.csv("D:/Binary_values.csv")
View(kariki_binary_values)

kariki_binary_values$RULE.INSTANCES<-NULL

#####Fitting a GLM model on the kariki_farm data without RST and ARM detection########
#' I did not convert the Rain factor into Character factor of Yes/No

kariki_original_model1<-glm(Rain~., data = kariki_farm2,family = "binomial",maxit = 100)
summary(kariki_original_model1)
anova(kariki_original_model1,test = 'LRT')
#mae(kariki_farm2$Rain,predict(kariki_original_model1))

plot_model(kariki_original_model1,vline.color = "green",sort.est = TRUE,show.values = TRUE)


anova(kariki_original_model1,kariki_binary_model1)
#### Fitting the GLM model and assessing it's viability ############
kariki_binary_model1 <- glm(Rain~.,data = kariki_binary_values,family = "binomial",maxit = 100)
summary(kariki_binary_model1)

predict(kariki_binary_model1,type = 'response')
anova(kariki_binary_model1, test = 'LRT')
mae(kariki_binary_values$Rain,predict(kariki_binary_model1))
rmse(kariki_binary_values$Rain,predict(kariki_binary_model1))
##
#'When running the anova test we find that Low_Temp[ 14.5,15.9]; Dewpoint_High[ 15.8,22.4]; Dewpoint_low[ 8.4,10.9];and Humidity_High..90.99.
#' are important predictors when it comes to Rain. High humidity has a high impact when it comes to Rain.
#' We also note the AIC value has droped till 50

c(extractAIC(kariki_original_model1), extractAIC(kariki_binary_model1)) # Shows the model with the binary values is prefered for use.
plot_model(kariki_binary_model1,vline.color = "red",sort.est = TRUE,show.values = TRUE)







