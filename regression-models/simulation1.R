n <- 100
x1 <- c(runif(n))
x2 <- rep(c(0,1), c(n/2, n/2))
beta0 <- 0
beta1 <- -2
beta2 <- 8
sigma <- .5
y <- beta0 + x1 * beta1 + x2 * beta2 + rnorm(n, sd=sigma)
df <- data.frame(x1, x2, y)
plot(x1, y, type="n", frame=FALSE)
# abline(lm(y ~ x), lwd=2)
# abline(h=mean(y[1:(n/2)]), lwd=3)
# abline(h=mean(y[(n/2 + 1):n]), lwd=3)
fit <- lm(y ~ x1 + x2, df)
abline(coef(fit)[1], coef(fit)[2], lwd=3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd=3)
points(x1[1:(n/2)], y[1:(n/2)], pch=21, col="black", bg="lightblue", cex=2)
points(x1[(n/2+1):n], y[(n/2+1):n], pch=21, col="black", bg="salmon", cex=2)

summary(fit)$coef