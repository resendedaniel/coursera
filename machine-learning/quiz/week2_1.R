set.seed(32343)
library(caret)
library(kernlab)
data(spam)

in_train <- createDataPartition(spam$type, p=.75, list=F)
training <- spam[in_train,]
testing <- spam[-in_train,]

qplot(capitalAve, data=training)

train_capitalAve <- training$capitalAve
train_capitalAveS <- (train_capitalAve - mean(train_capitalAve)) / sd(train_capitalAve)
mean(train_capitalAveS)
sd(train_capitalAveS)
qplot(train_capitalAveS)

preObj <- preProcess(dplyr::select(training, -type), method=c("center", "scale"),
                     verbose=T)
train_capitalAveS <- predict(preObj, dplyr::select(training, -type))$capitalAve
mean(train_capitalAveS)
sd(train_capitalAveS)
qplot(train_capitalAveS)

test_capitalAve <- testing$capitalAve
test_capitalAveS <- (test_capitalAve - mean(train_capitalAve)) / sd(train_capitalAve)
mean(test_capitalAveS)
sd(test_capitalAveS)
qplot(test_capitalAveS)

test_capitalAveS <- predict(preObj, dplyr::select(testing, -type))$capitalAve
mean(test_capitalAveS)
sd(test_capitalAveS)
qplot(test_capitalAveS)

model_fit <- train(type ~ ., data=training,
                  preProcess=c("center", "scale"), method="glm")
model_fit

preObj <- preProcess(dplyr::select(training, -type), method="BoxCox")
train_capitalAveS <- predict(preObj, dplyr::select(training, -type))$capitalAve
par(mfrow=c(1,2)); hist(train_capitalAveS); qqnorm(train_capitalAveS); par(mfrow=c(1,1))