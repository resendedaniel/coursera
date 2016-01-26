library(UsingR)
data(diamond)

y <- diamond$price
x <- diamond$carat
n <- length(y)
fit <- lm(y ~ x)

beta1 <- cor(y, x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
e <- y - beta0 - beta1 * x
sigma <- sqrt(sum(e^2) / (n-2)) 
ssx <- sum((x - mean(x))^2)
seBeta0 <- (1 / n + mean(x) ^ 2 / ssx) ^ .5 * sigma 
seBeta1 <- sigma / sqrt(ssx)
tBeta0 <- beta0 / seBeta0; tBeta1 <- beta1 / seBeta1
pBeta0 <- 2 * pt(abs(tBeta0), df = n - 2, lower.tail = FALSE)
pBeta1 <- 2 * pt(abs(tBeta1), df = n - 2, lower.tail = FALSE)
coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0), c(beta1, seBeta1, tBeta1, pBeta1))
colnames(coefTable) <- c("Estimate", "Std. Error", "t value", "P(>|t|)")
rownames(coefTable) <- c("(Intercept)", "x")

sumCoef <- summary(fit)$coefficients
sumCoef[2,1] + c(-1,1) * qt(.975, df=fit$df) * sumCoef[2,2]

xVals <- seq(min(x), max(x), by=.01)
newData <- data.frame(x=xVals)
p1 <- predict(fit, newData, interval=("confidence"))
p2 <- predict(fit, newData, interval=("prediction"))
plot(x, y, pch=20, frame=F)
abline(fit, lty=2)
lines(xVals, p1[,2], lty=3)
lines(xVals, p1[,3], lty=3)
lines(xVals, p2[,2], lty=3)
lines(xVals, p2[,3], lty=3)

err <- predict(fit, newData, se=TRUE)
nsd <- .95
ucl <- err$fit + qt(nsd, df=err$df) * err$se.fit
lcl <- err$fit - qt(nsd, df=err$df) * err$se.fit
grid <- data.frame(x=xVals, y=err$fit, ucl, lcl)

# ggplot(data.frame(x=x, y=y), aes(x, y)) +
#     geom_point() +
#     geom_smooth(aes(ymin = lcl, ymax = ucl), data=grid, stat="identity")
