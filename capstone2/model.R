library(capstone2)

version <- "001"
sample_size <- 2000
set.seed(9483)

# Load
data <- loadData()
data <- do.call(c, data)
data <- sample(data, sample_size)

# Have to create a data.frame with prediction and outcome
# Choosing two or three words and getting the nex one
# Fuck regex

# Partition
train_ind <- caret::createDataPartition(y = data, p = 0.1, list = FALSE)
train <- data[train_ind]
test <- data[-train_ind]
