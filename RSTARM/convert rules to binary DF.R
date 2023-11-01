library(arules)
data("Groceries")
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))
# Create a list of all unique items in the rules
# Get a list of all items
items <- unique(unlist(lapply(1:length(rules), function(i) c(lhs = labels(lhs(rules[i])), rhs = labels(rhs(rules[i]))))))

# Create a binary matrix
binaryMatrix <- matrix(0, nrow = length(rules), ncol = length(items), dimnames = list(NULL, items))

# Fill matrix with 1s where items are in rules
for (i in seq_len(length(rules))) {
  rule_items <- c(lhs = labels(lhs(rules[i])), rhs = labels(rhs(rules[i])))
  binaryMatrix[i, rule_items] <- 1
}

# Convert to data frame
binary_df <- as.data.frame(binaryMatrix)

# Set the seed to make your partition reproducible
set.seed(123)

# Split the data into training and test sets
train_indices <- sample(1:nrow(binary_df), 0.7 * nrow(binary_df))
train_set <- binary_df[train_indices, ]
test_set <- binary_df[-train_indices, ]

# Fit the model to the training data
model <- glm({bottled beer} ~ ., data = train_set, family = binomial)