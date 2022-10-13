## Checking into discretizing the precipitation amount attribute which has alot of zero's

library(funModeling)
library(dplyr)
library(ggplot2)
k_farm <- read_sheet("https://docs.google.com/spreadsheets/d/1y29ch-sv9UXSZUX9mRxqx6NleN6-XO3ifXDqkDzeOiE/edit#gid=0")

(n<-nrow(k_farm)) # Checking number of rows in the data which is 1619 observations with 17 varibales
c(as.character(k_farm$Date[1]), as.character(k_farm$Date[n])) # the date range from 2/3/2018 to 18/8/2022
head(k_farm$Rain)
k_farm$Rain <- factor(k_farm$Rain) # Rain Yes/No to factor
k_farm$Date <- as.Date(k_farm$Date, '%m/%d/%Y') # Date column to date
str(k_farm)

####Using the fun modelling
k_bins <- discretize_get_bins(data = k_farm,input = "Precipitation_amount", n_bins = 3)

# Now it can be applied on the same data frame or in a new one (for example, in a predictive model that changes data over time)
k_dicretized=discretize_df(data=k_farm, data_bins=k_bins, stringsAsFactors=T)

describe(k_dicretized%>%select(Precipitation_amount))

precip_amt=ggplot(k_dicretized, aes(Precipitation_amount)) + geom_bar(fill="#0072B2") + theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))