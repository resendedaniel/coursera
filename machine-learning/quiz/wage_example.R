library(ISLR)
library(caret)
library(splines)
data(Wage)
Wage <- dplyr::select(Wage, -logwage)

inTrain <- createDataPartition(y=Wage$wage, p=.7, list=F)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

featurePlot(x=training[, c("age", "education", "jobclass")],
            y=training$wage,
            plot="pairs")

qplot(age, wage, data=training) + geom_smooth()
qplot(age, wage, colour=jobclass, data=training)
qplot(age, wage, colour=education, data=training)

modFit <- train(wage ~ age + jobclass + education, method="lm", data=training)
finMod <- modFit$finalModel
modFit

plot(finMod, 1, pch=19, cex=.5, col="#00000032")
abline(h=0)

qplot(finMod$fitted, finMod$residuals, colour=race, data=training)
plot(finMod$residuals, pch=20)

pred <- predict(modFit, testing)
qplot(wage, pred, colour=year, data=testing, smooth="lm") + geom_smooth(method="lm")

modFitAll <- train(wage ~ ., data=training, method="lm")
pred <- predict(modFitAll, testing)
qplot(testing$wage, pred) + geom_smooth(method="lm")
