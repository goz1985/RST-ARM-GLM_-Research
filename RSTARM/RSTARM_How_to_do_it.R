library(RoughSets)
#' Load the Rough Set library which comes with the test data set for hiring where a decision maker will decide whether-
#' to employ a person based on their diploma qualification, reference or experience.
#' The work involves deducing important attributes from the data set using the indiscernibility matrix
#' rough sets and discerniblity matrix computations.
data("RoughSetData")
attr <- c(1,2,3)
decision.table<-RoughSetData$hiring.dt
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

IND <- BC.IND.relation.RST(decision.table, feature.set = attr)
#Computation of upper and lower approximations to check with attributes affect the decision variable well and which dont.

roughset <- BC.LU.approximation.RST(decision.table, IND)
region.RST <- BC.positive.reg.RST(decision.table, roughset)
disc.mat <- BC.discernibility.mat.RST(decision.table, range.object = NULL)

decision_table_hiring <- FS.all.reducts.computation(disc.mat)
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

library(arules)
AR_table_1 <- as.data.frame(new_hiring_table1 )
View(AR_table_1)
# Convert the Reducts into transactions. The transactions help identify strong patterns and relationships among the patterns in the datasets.
tdata <- as(AR_table_1,"transactions")
View(tdata)

#Rule generation using ARM
rules <- apriori(tdata,parameter = list(minlen=2,supp=0.005,conf=0.8),appearance = list(rhs=c("Decision=Reject","Decision=Accept"),default="lhs"))
inspect(rules)

# Dataframe for the rules generated
rules_df <- data.frame(lhs=labels(lhs(rules)),rhs=labels(rhs(rules)))
View(rules_df)
rules_df <- data.frame(lhs=labels(lhs(rules)),rhs=labels(rhs(rules)),rules@quality)

# Generating a good binary data frame
