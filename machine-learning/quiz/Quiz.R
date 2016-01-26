# Quiz 2

## 1
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData = data.frame(diagnosis, predictors)
trainIndex = createDataPartition(diagnosis,p=0.5, list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]


## 2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]


## 3
library(caret)
library(AppliedPredictiveModeling)
library(dplyr)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis, predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]


preProc <- preProcess(training[grep("IL", names(predictors))], 
                      method="pca",
                      thresh=.9)
trainPC <- predict(preProc, training[grep("IL", names(predictors))])
modelFit <- train(training$diagnosis ~ ., 
                  method="glm",
                  preProcess="pca",
                  data = training[grep("IL", names(predictors))])
testPC <- predict(preProc, testing[grep("IL", names(predictors))])
confusionMatrix(testing$diagnosis, predict(modelFit, testPC))


modAll <- train(diagnosis ~ ., 
                data=dplyr::select(training, c(1, grep("IL", names(predictors)))),
                method="glm",
                preProcess = c("pca"))


## 4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
