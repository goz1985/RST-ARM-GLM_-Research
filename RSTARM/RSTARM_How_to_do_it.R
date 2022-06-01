library(RoughSets)
data("RoughSetData")
attr <- c(1,2,3)
IND <- BC.IND.relation.RST(decision.table, feature.set = attr.P)
decision.table<-RoughSetData$hiring.dt
<<<<<<< HEAD
hiring.data <- decision.table[sample(nrow(decision.table)),]

# Splitting the dataset
hdx <- round(0.7*nrow(hiring.data))
hiring_training <- SF.asDecisionTable(hiring.data[1:hdx,],decision.attr = 5,indx.nominal = 5 )
hiring_testing <- SF.asDecisionTable(hiring.data[(hdx+1): nrow(hiring.data),-ncol(hiring.data)])

#' 1. Indiscernible computation
#' 2. Rough Set computation
#' 3. Boundary computation
#' 4. Decision Rule formulation
IND_1 <- BC.IND.relation.RST(hiring_training, feature.set = attr)
RST_1 <- BC.LU.approximation.RST(hiring_training,IND_1)
Region_1 <- BC.positive.reg.RST(hiring_training,RST_1)
disc_1 <- BC.discernibility.mat.RST(hiring_training,range.object = NULL)
decision_hiring_1 <- FS.all.reducts.computation(disc_1)
new_h1 <- SF.applyDecTable(hiring_training,decision_hiring_1,control = list(indx.reduct = 1))
new_h2 <- SF.applyDecTable(hiring_training,decision_hiring_1,control = list(indx.reduct = 2))

# Rule generation using roughsets
rules_1 <- RI.CN2Rules.RST(hiring_training, K = 5)
rules_1 
as.list(rules_1)
pred.vals <- predict(rules_1, new_h1)
pred.vals

mean(pred.vals)

#Indiscernibility Computation.

=======
IND <- BC.IND.relation.RST(decision.table, feature.set = attr.P)
>>>>>>> 5fde6696ade2ffe214ac4d06509120803ca9fec3
IND <- BC.IND.relation.RST(decision.table, feature.set = attr)
roughset <- BC.LU.approximation.RST(decision.table, IND)
region.RST <- BC.positive.reg.RST(decision.table, roughset)
disc.mat <- BC.discernibility.mat.RST(decision.table, range.object = NULL)
decision_table_hiring <- FS.all.reducts.computation(disc.mat)
<<<<<<< HEAD
# From the above reduct computation we get two Reduct each having different attributes in the tables
# Table one the attributes are : "Diploma", "Experience, "Decision"
# Table two the attributes are : "Experience", "Reference", "Decision"
new_hiring_table1 <- SF.applyDecTable(decision.table,decision_table_hiring,control = list(indx.reduct = 1))
new_hiring_table2 <- SF.applyDecTable(decision.table,decision_table_hiring,control = list(indx.reduct = 2))

# Using the greedy heuristic method to generate reduct
# Setting the epsilon value to rep an approximate threashold..Compute approximate reducts or not
decision_hiring <- FS.greedy.heuristic.reduct.RST(decision.table,qualityF = X.entropy,epsilon = 0.9)
Hiring_table <- SF.applyDecTable(decision.table,decision_hiring)




# Loading the association rule mining package for formulating the decision rules for the above dataset.

=======
new_hiring_table <- SF.applyDecTable(decision.table,decision_table_hiring,control = list(indx.reduct = 1))
install.packages("arules")
>>>>>>> 5fde6696ade2ffe214ac4d06509120803ca9fec3
library(arules)
library(arules)
AR_table <- as.data.frame(new_hiring_table )
View(AR_table)
tdata <- as(AR_table,"transactions")
View(tdata)
rules <- apriori(tdata,parameter = list(minlen=2,supp=0.005,conf=0.8),appearance = list(rhs=c("Decision=Reject","Decision=Accept"),default="lhs",control=list(verbose=F)))
rules <- apriori(tdata,parameter = list(minlen=2,supp=0.005,conf=0.8),appearance = list(rhs=c("Decision=Reject","Decision=Accept"),default="lhs"))
inspect(rules)
rules_df <- data.frame(lhs=labels(lhs(rules)),rhs=labels(rhs(rules)))
View(rules_df)
rules_df <- data.frame(lhs=labels(lhs(rules)),rhs=labels(rhs(rules)),rules@quality)
