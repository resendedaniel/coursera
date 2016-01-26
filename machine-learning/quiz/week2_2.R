library(ISLR)
library(caret)
library(splines)
data(Wage)

inTrain <- createDataPartition(y=Wage$wage, p=.7, list=F)
training <- Wage[inTrain,]
testing <- Wage[-inTrain]

table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass, data=training)
head(predict(dummies, newdata=training))

nsv <- nearZeroVar(training, saveMetrics=T)
nsv

bsBasis <- bs(training$age, df=3)
bsBasis

lm1 <- lm(wage ~ bsBasis, data=training)
summary(lm1)
plot(training$age, training$wage, pch=19, cex=.5)
points(training$age, predict(lm1, newdata=training), col="red", pch=19, cex=.5)

predict(bsBasis, age=testing$age)
