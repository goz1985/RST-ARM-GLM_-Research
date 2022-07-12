###### Creating association rules on the data set without taking it through the Roughset Process###########

library(googlesheets4)
library(arules)
library(dplyr)
library(RColorBrewer)
library(arulesViz)

k_farm <- read_sheet("https://docs.google.com/spreadsheets/d/1y29ch-sv9UXSZUX9mRxqx6NleN6-XO3ifXDqkDzeOiE/edit#gid=0")

(n<-nrow(k_farm)) # Checking number of rows in the data which is 1179
c(as.character(k_farm$Date[1]), as.character(k_farm$Date[n])) # the date range from 2/3/2018 to 20/5/2021
head(k_farm$Rain)
k_farm$Rain <- factor(k_farm$Rain) # Rain Yes/No to factor
k_farm$Date <- as.Date(k_farm$Date, '%m/%d/%Y') # Date column to date
str(k_farm)

view(k_farm)
(cols_withNa <- apply(k_farm, 2, function(x) sum(is.na(x))))# Getting missing value tabulation

######## Picking only complete values, I create another data frame kariki_farm2################I removed missing value rows in the dataset
kariki_farm2 <- k_farm[complete.cases(k_farm),]
str(kariki_farm2)
(cols_withNa <- apply(kariki_farm2, 2, function(x) sum(is.na(x))))
kariki_farm2$Date <- NULL # removing the date column
kariki_farm2$Windspeed_low <- NULL # removing windsped low as it has no effect on the prediction 
kariki_farm2$Rain <- factor(kariki_farm2$Rain, labels = c("No","Yes"))

#### Removing the precipitation amount because it will influence the outcome variable#########
kariki_farm2$Precipitation_amount<- NULL
#############################################################################################################


###Applying association rule mining method###########################################################
k_farm <- as.data.frame(kariki_farm2)

### Discretizing the values as without discretizing them the arule method wont work..values must be nominal####
k_farm$High_Temp<-discretize(k_farm$High_Temp)
k_farm$Avg_Temp<-discretize(k_farm$Avg_Temp)
k_farm$Low_Temp<-discretize(k_farm$Low_Temp)
k_farm$Dewpoint_High<-discretize(k_farm$Dewpoint_High)
k_farm$Dewpoint_Avg<-discretize(k_farm$Dewpoint_Avg)
k_farm$Dewpoint_low<-discretize(k_farm$Dewpoint_low)
k_farm$Humidity_High<-discretize(k_farm$Humidity_High)
k_farm$Humidity_Avg<-discretize(k_farm$Humidity_Avg)
k_farm$Humidity_Low<-discretize(k_farm$Humidity_Low)
k_farm$Windspeed_High<-discretize(k_farm$Windspeed_High)
k_farm$Windspeed_Avg<-discretize(k_farm$Windspeed_Avg)
k_farm$`High(Hpa)`<-discretize(k_farm$`High(Hpa)`)
k_farm$`Low(Hpa)`<-discretize(k_farm$`Low(Hpa)`)
kfarm_trans <- as(k_farm,"transactions")

itemLabels(kfarm_trans)
