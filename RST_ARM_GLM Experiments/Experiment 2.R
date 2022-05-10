library(RoughSets)
kariki_farm <- read.csv("D:/datasets/Kariki_Farm1.csv", stringsAsFactors = FALSE, fileEncoding="latin1")
c(as.character(kariki_farm$Date[1]), as.character(kariki_farm$Date[n]))
kariki_farm$Rain <- factor(kariki_farm$Rain)
kariki_farm$Date <- as.Date(kariki_farm$Date, '%m/%d/%Y')
(cols_withNa <- apply(kariki_farm, 2, function(x) sum(is.na(x))))
kariki_farm2 <- kariki_farm[complete.cases(kariki_farm),]
str(kariki_farm2)
(cols_withNa <- apply(kariki_farm2, 2, function(x) sum(is.na(x))))
kariki_farm2$High.Hpa. <- as.numeric(kariki_farm2$High.Hpa.)
kariki_farm2$Low.Hpa. <- as.numeric(kariki_farm2$Low.Hpa.)
kariki_farm2$Date <- NULL

#' Loading of dataset(kariki farm) and preprocessing via removal of null values and only considering
#' complete cases.
#' Now doing doing feature selection and approximation with RST
#' I will have to choose whether to split training and testing sets or do whole cross validation

set.seed(123)
weather_Shuffled <- kariki_farm2[sample(nrow(kariki_farm2)),]
split <- round(0.75* nrow(weather_Shuffled))
train <- SF.asDecisionTable(weather_Shuffled[1:split,],decision.attr = 16, indx.nominal = 16)
test <- SF.asDecisionTable(weather_Shuffled[(split+1):nrow(weather_Shuffled),-ncol(weather_Shuffled)])

# Discretization of the split training and testing sets
cut_vaues <- D.discretization.RST(train, type.method = "local.discernibility")

kariki_train <- SF.applyDecTable(train,cut_vaues)
kariki_test <- SF.applyDecTable(test,cut_vaues)

# Feature selection
kariki_rst <- FS.feature.subset.computation(kariki_train,method = "quickreduct.rst")
kariki_rst_2 <- FS.feature.subset.computation(kariki_train,method = "greedy.heuristic.superreduct")

kariki_QR <- SF.applyDecTable(kariki_train,kariki_rst) # reduct containg precipitation amt and Rain factor
kariki_GHR <- SF.applyDecTable(kariki_train,kariki_rst_2) # reduct containg precipitation amt and Rain factor

########################################################################
#' Forming a decision table after performing indiscernibility matrix..reduct contains: Avg_dewpoint,high and low pressure, precipitation amt)
#' 
kariki_DT <- SF.asDecisionTable(dataset = kariki_farm2,decision.attr = 16,indx.nominal = 16)
kariki_cut_values <- D.discretization.RST(kariki_DT,type.method = "local.discernibility")
kariki_discretized <- SF.applyDecTable(kariki_DT,kariki_cut_values)
kariki_IND <- BC.IND.relation.RST(kariki_discretized,feature.set = NULL)
Kariki_RST <- BC.LU.approximation.RST(kariki_discretized,kariki_IND)
kariki_dic_matrix <- BC.discernibility.mat.RST(kariki_discretized,return.matrix = TRUE)
kariki_reduct <- FS.one.reduct.computation(kariki_dic_matrix)
new_kariki_table <- SF.applyDecTable(kariki_DT,kariki_reduct,control = list(indx.reduct=1))
#' After preprocessing and forming the roughset for detection of key terms, I set out to test it on a glm model

glm_RSt <- glm(Rain ~.,family = binomial(link = "logit"), data=new_kariki_table, maxit = 100)
printCoefmat(coef(summary(glm_full)))
printCoefmat(coef(summary(glm_RSt)))

glm_3 <- glm(Precipitation_amount~., family = gaussian, data = kariki_farm2, maxit= 100)# checking on the aspects that affect amount of rainfall
glm_4 <- glm(Precipitation_amount~., family = gaussian, data = new_kariki_table, maxit= 100)
confint(glm_RSt)
confint(glm_3)
confint(glm_4)
