# Quiz 3

## 1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

data <- split(segmentationOriginal, segmentationOriginal$Case)

set.seed(125)
model <- train(Class ~ ., data=data$Train, method="rpart")


## 2
library(pgmm)
data(olive)
olive = olive[,-1]

model <- train(Area ~ ., data=olive)

newdata = as.data.frame(t(colMeans(olive)))
predict(model, newdata)

## 4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
model <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
               data=trainSA, 
               method="glm",
               family="binomial")

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
prediction <- predict(model, testSA)
missClass(trainSA, prediction)

## 5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)

set.seed(33833)
model <- randomForest(y ~ ., data=vowel.train)
varImp(model)
