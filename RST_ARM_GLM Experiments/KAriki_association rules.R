###### Creating association rules on the kariki_farm data set without taking it through the RoughSet Process###########

library(googlesheets4)
library(arules)
library(dplyr)
library(RColorBrewer)
library(arulesViz)
library(plyr)
library(funModeling)
library(dplyr)
library(ggplot2)

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
##also due to issues with discretizing the element####

#############################################################################################################

#########Using the fun modelling to discretize the precipitation amount if to use it in my work#######
Kariki_bins <- discretize_get_bins(data = kariki_farm2,input = "Precipitation_amount", n_bins = 10)

# Now it can be applied on the same data frame or in a new one (for example, in a predictive model that changes data over time)
kariki_farm2=discretize_df(data=kariki_farm2, data_bins=Kariki_bins, stringsAsFactors=T)

describe(kariki_farm2%>%select(Precipitation_amount))

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

itemFrequencyPlot(kfarm_trans,topN=20,type='absolute')

kfarm_freq_item_set <- eclat (kfarm_trans, parameter = list(supp = 0.02, maxlen = 15))
inspect(kfarm_freq_item_set)


#########################Rule generation###################################################
### Will have to get the most optimal rule generation values.#####

K_rules2 <- apriori(kfarm_trans,parameter = list(minlen = 2,supp= 0.05, conf = 0.8, maxlen=15),appearance = list(rhs= c("Rain=Yes", "Rain=No")))

k_rules_table <- data.frame(lhs=labels(lhs(K_rules2)),rhs=labels(rhs(K_rules2)),K_rules2@quality)

###Pruning rules which are subset of other rules, originally there were 51631 rules, after pruning they are now  rules#######
subset.k_rules<-is.subset(K_rules2)
subset.k_rules

subset.k_rules[lower.tri(subset.k_rules, diag=T)] <- F
subset.k_rules

redundant<- apply(subset.k_rules,2,any)
redundant

rules.pruned_1 <- K_rules2[!redundant]
inspect(rules.pruned_1)

interestMeasure(rules.pruned, c("support", "chiSquare", "confidence", "conviction", "cosine", "coverage", "leverage", "lift", "oddsRatio"), k_farm)

########Visualize the rules via graphs to see the distribution of the rules according to the classes######
plot(rules.pruned_1, jitter = 0)

plot(rules.pruned_1, method="grouped")

plot(rules.pruned_1, method="graph")

######Putting the pruned rules in a data frame##########################
kariki_pruned_rules <- data.frame(lhs=labels(lhs(rules.pruned_1)),rhs=labels(rhs(rules.pruned_1)),rules.pruned_1@quality)

write.csv(kariki_pruned_rules,"D:\\Phd Research\\pruned_rules.csv", row.names = FALSE)


###Need to prine the rules further to get the rules with the highest confidence, lift and support, then convert them into a decision frame#####``