library(RoughSets)
#' Load the Rough Set library which comes with the test data set for hiring where a decision maker will decide whether-
#' to employ a person based on their diploma qualification, reference or experience.
#' The work involves deducing important attributes from the data set using the indiscernibility matrix
#' rough sets and discerniblity matrix computations.
data("RoughSetData")
attr <- c(1,2,3)
decision.table<-RoughSetData$hiring.dt
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
