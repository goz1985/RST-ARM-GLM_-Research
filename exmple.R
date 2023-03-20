data(RoughSetData)
dataset <- RoughSetData$wine.dt
set.seed(5)
dt.Shuffled <- dataset[sample(nrow(dataset)),]
idx <- round(0.8 * nrow(dt.Shuffled))
wine.tra <-SF.asDecisionTable(dt.Shuffled[1:idx,],decision.attr = 14, indx.nominal = c(1:14))
wine.tst <- SF.asDecisionTable(dt.Shuffled[(idx+1):nrow(dt.Shuffled), -ncol(dt.Shuffled)])
cut.values <- D.discretization.RST(wine.tra,type.method = "unsupervised.quantiles")  
red.rst <- FS.feature.subset.computation(wine.tra,method="quickreduct.rst")
fs.tra <- SF.applyDecTable(wine.tra, red.rst)                                         
rules <- RI.indiscernibilityBasedRules.RST(wine.tra, red.rst)
pred.vals <- predict(rules, wine.tst)                                          

pred.vals <- predict(rules, d.tst)

## calculating error
real.val <- dt.Shuffled[(idx+1):nrow(dt.Shuffled),ncol(dt.Shuffled), drop = FALSE]
err.1 <- 100*sum(pred.vals!=real.val)/nrow(pred.vals)                       
cat("The percentage error = ", err.1, "\n")

RI.laplace(rules)
RI.confidence(rules)

RI.support(rules) 