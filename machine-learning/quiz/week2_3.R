library(caret)
library(gridExtra)
data(faithful)
set.seed(333)

inTrain <- createDataPartition(y=faithful$waiting, p=.5, list=F)
training <- faithful[inTrain,]
testing <- faithful[-inTrain,]

lm1 <- lm(eruptions ~ waiting, training)
summary(lm1)
pred1 <- predict(lm1, newdata=testing, interval="prediction")
pred1 <- data.frame(cbind(waiting=testing$waiting, pred1))

modFit <- train(eruptions ~ waiting, data=training, method="lm")
summary(modFit$finalModel)

head(training)
g_training <- ggplot(training, aes(waiting, eruptions)) +
    geom_point() +
#     geom_smooth(method="lm") +
#     geom_line(data=data.frame(waiting=training$waiting, eruptions=lm1$fitted), aes(waiting, eruptions)) +
    geom_line(data=melt(pred1, id="waiting"), aes(waiting, value, color=variable)) +
    xlab("Waiting") + 
    ylab("Duration")

g_testing <- ggplot(testing, aes(waiting, eruptions)) +
    geom_point() +
    geom_line(data=melt(pred1, id="waiting"), aes(waiting, value, color=variable)) +
    #     geom_line(data=data.frame(waiting=training$waiting, eruptions=lm1$fitted), aes(waiting, eruptions)) +
    xlab("Waiting") + 
    ylab("Duration")

grid.arrange(g_training, g_testing, nrow=2)


# Calculating the error RMSE
sqrt(sum((lm1$fitted - training$eruptions)^2))
sqrt(sum((predict(lm1, newdata=testing) - testing$eruptions)^2))
