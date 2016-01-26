library(dplyr)
library(caret)
library(ElemStatLearn)

# Quiz 4

## 1 Boosting
data(vowel.train)
data(vowel.test)

vowel.train <- mutate(vowel.train, y = factor(y))
vowel.test <- mutate(vowel.test, y = factor(y))

set.seed(33833)
model_rf <- train(y ~., data=vowel.train, method="rf")
model_gbm <- train(y ~., data=vowel.train, method="gbm", verbos=F)
print(model_rf)
print(model_gbm)

mean(predict(model_rf, newdata=vowel.test) == vowel.test$y)
mean(predict(model_gbm, newdata=vowel.test) == vowel.test$y)
mean(predict(model_gbm, newdata=vowel.test) == predict(model_rf, newdata=vowel.test))

## 2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
model1 <- train(diagnosis ~ ., data=training, method="rf")
model2 <- train(diagnosis ~ ., data=training, method="gbm")
model3 <- train(diagnosis ~ ., data=training, method="lda")
predictor1 <- predict(model1, newdata=testing)
predictor2 <- predict(model2, newdata=testing)
predictor3 <- predict(model3, newdata=testing)

mean(testing$diagnosis == predictor1)
mean(testing$diagnosis == predictor2)
mean(testing$diagnosis == predictor3)


## 3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)
model <- train(CompressiveStrength ~ ., data=training, method="lasso")


## 4
library(lubridate)  # For year() function below

dat = read.csv(url("http://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"), stringsAsFactors=F)
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

tsrem = ts(testing$visitsTumblr)

model = bats(tstrain)

pred <- forecast(model, h=length(tsrem),level=c(95))

accuracy(pred, testing$visitsTumblr)
acc = sum(testing$visitsTumblr <= pred$upper) / nrow(testing)


## 5
library(e1071)

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(325)
svmFit <- svm(CompressiveStrength ~ ., data = training)
svmPred <- predict(svmFit,testing)
accuracy(svmPred, testing$CompressiveStrength)




