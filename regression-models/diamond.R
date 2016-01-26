library(UsingR)
## Plot
localPlot <- function (x, y, fit) {
    ic <- coef(fit)[1]
    slope <- coef(fit)[2]
    plot(x, y, pch=20,
         main=paste("slope: ", round(slope,2), "\n",
                    "intercept 0:", round(ic, 2)))
    abline(fit)
    abline(h=mean(y), v=mean(x), lty=3)
    for(i in seq_along(fit$residuals)) {
        segments(x0=x[i], y0=x[i]*slope + ic, y1=y[i], lty=2)
    }
}















## Data
data(diamond)
x <- diamond$carat
y <- diamond$price
n <- length(y)

## Linear model
fit <- fit1 <- lm(price ~ carat, data=diamond)
# fit2 <- lm(price ~ I(carat - mean(carat)), data=diamond)
# par(mfrow=c(2,1))
localPlot(x, y, fit1)
# localPlot(x - mean(x), y, fit2)
ic <- coef(fit)[1]
slope <- coef(fit)[2]
slope <- cov(x, y) / var(x)

## Summary
print(summary(diamond))
print(fit1)
# print(fit2)

## Predict
randomCarat <- rnorm(25, mean=mean(x), sd=sd(x))
randomCarat <- randomCarat[randomCarat>0]
pred <- predict(fit1, newdata=data.frame(carat=randomCarat))
points(randomCarat, pred, pch=20, col="red")

## Residual
e <- resid(fit)
yhat <- predict(fit)
plot(e, pch=20, lwd=0)
abline(h=0, lty=2)
points((y - yhat), col="red")

### Residual Sigma
summary(fit)$sigma
e <- resid(fit)
sqrt(sum((1/(n-2))*(e^2)))
