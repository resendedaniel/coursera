library(caret)
library(kernlab)
data(spam)

in_train <- createDataPartition(spam$type, p=.75, list=F)
training <- spam[in_train,]
testing <- spam[-in_train,]

M <- abs(cor(training[,-58]))
diag(M) <- 0
which(M > .8, arr.ind=T)