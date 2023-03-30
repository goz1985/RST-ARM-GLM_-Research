#__________________ Modelling the Titanic solution__________________________
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
library(arules)
library(arulesViz)
#-----Loading the dataset-------------------------------------------------------
titanic_df <- read.csv("D:/Datasets/Datasets for testing on model/titanic.csv")
head(titanic_df)
str(titanic_df)

sapply(titanic_df, function(x) length(unique(x)))
missing_values <- titanic_df%>% summarise_all(funs(sum(is.na(.))/n()))
missing_values <- gather(missing_values, key="feature", value="missing_pct")
missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red")+
  theme_bw() + coord_flip()

titanic_df <- titanic_df %>% mutate(Age_Group = case_when(Age < 13 ~ "Age.0012", 
                                              Age >= 13 & Age < 18 ~ "Age.1317",
                                              Age >= 18 & Age < 60 ~ "Age.1859",
                                              Age >= 60 ~ "Age.60Ov"))
titanic_df$Embarked <- replace(titanic_df$Embarked, which(is.na(titanic_df$Embarked)), "S")
#................ Tidying up the dataset........................................
titanic_df$Survived<-as.factor(titanic_df$Survived)
titanic_df$Pclass<-as.factor(titanic_df$Pclass)
titanic_df$Sex<-as.factor(titanic_df$Sex)
titanic_df$Embarked<-as.factor(titanic_df$Embarked)
str(titanic_df)

titanic_df$PassengerId<-NULL
titanic_df$Age_Group<-NULL
titanic_df$Name<-NULL
titanic_df$Ticket<-NULL
titanic_df$Cabin<-NULL

(cols_withNa <- apply(titanic_df, 2, function(x) sum(is.na(x))))# Getting missing value tabulation, Age has 86

age.median <- median(titanic_df$Age,na.rm = TRUE)
titanic_df[is.na(titanic_df$Age),"Age"]<-age.median




#...........................Modelling the dataset on key models such as decision tree and such......................
library(dplyr)
library(Metrics)
library(caret)
library(caretEnsemble)

set.seed(1)
titanic_partition<- createDataPartition(titanic_df$Survived, p =0.75, list = FALSE, times = 1)
titanic_train <- titanic_df[titanic_partition,]
titanic_test <- titanic_df[-titanic_partition,]

titanic_ctrl <- trainControl(method="repeatedcv", number=10, repeats=20,summaryFunction = twoClassSummary,
                           savePredictions=TRUE, classProbs=TRUE,preProc = c("center","scale"))
#----------------------Decision Tree-------------------------------------------------------------------------------
library(rpart)
library(rpart.plot)
titanic_tree <-rpart(Survived~.,data = titanic_train)
summary(titanic_tree)
titanic_tree_plot <-rpart.plot(titanic_tree)
titanic_tree_predict<-predict(titanic_tree,titanic_test)
confusionMatrix(titanic_tree_predict,titanic_test$Survived,positive = '1')

#...................................GLM..........................................................................
glm_titanic<-glm(Survived ~., data = titanic_train,family = binomial,maxit=100)
summary(glm_titanic)
anova(glm_titanic,test = 'Chisq')

#................................... Rough Set Theory.............................................................
titanic_shuffled <- titanic_df[sample(nrow(titanic_df)),]
titanic_decision_table <- SF.asDecisionTable(titanic_shuffled,decision.attr = 1, indx.nominal = c(1))

titanic.cut.values <- D.discretization.RST(titanic_decision_table,type.method = "local.discernibility")
