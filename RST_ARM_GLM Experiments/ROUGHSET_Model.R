# Dealing with KAriki farm using the rough set model and formulation of decision rules
# In this notebook, I will be using roughset theory on the kariki dataset to reduce the set of terms.
# Also I will want to see whether having a discretized dataset is better than having the original data.
# Which discretization methods are better

# The notebook will start by loading neccessary libraries, loading data, preprocessing it and removing null values
# Will also want to see whether I can do imputation on the data

library(RoughSets)
library(RoughSetKnowledgeReduction)

#Loading the dataset and preprocessing it
kariki_farm <- read.csv("E:/Datasets/Research datasets/Weather data/Kariki_Farm.csv")
(n<-nrow(kariki_farm)) # Checking number of rows in the data which is 1179
c(as.character(kariki_farm$Date[1]), as.character(kariki_farm$Date[n])) # the date range from 2/3/2018 to 20/5/2021
head(kariki_farm$Rain.Yes.No.)
kariki_farm$Rain <- factor(kariki_farm$Rain) # Rain Yes/No to factor
kariki_farm$Date <- as.Date(kariki_farm$Date, '%m/%d/%Y') # Date column to date
str(kariki_farm)# When looking at the high and low numeric pressure, I had to convert them to numeric because they were being considered as being factors yet theyr weren't

# Will have to check on the aspect of changing this to numeric from factor because it is giving weird values
kariki_farm$High.Hpa. <- as.numeric(kariki_farm$High.Hpa.)
kariki_farm$Low.Hpa. <- as.numeric(kariki_farm$Low.Hpa.)
str(kariki_farm)
view(kariki_farm)
(cols_withNa <- apply(kariki_farm, 2, function(x) sum(is.na(x))))

# Value imputation 
summary(kariki_farm) # done to get the overlay of the data.
# Creating a visual representation of the missing values
library(VIM)
Kariki_plot <- aggr(kariki_farm, col=c('navyblue','yellow'),numbers = TRUE, sortVars = TRUE, labels =names(kariki_farm),cex.axis=.7,gap=3, ylab=c("Missing data","Pattern"))

#Imputing the missing values using a suitable method. Still referring 

# Will use mice as the first method, then will try using HMsic (with the aregimpute()method)
library(mice)
library(missForest)
library(Hmisc) #Giving an error message while loading

# Imputation with MICE package
kariki_imputed <- mice(kariki_farm, m =1, method = "pmm", maxit = 50, seed = 123)
kariki_imputed_complete <- complete(kariki_imputed)

##### Dealing with the roughset
# 


