# Quiz 1

## 1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
z <- mapply(function(x, w) {
    rep(x, w)
}, x, w)
z <- unlist(z)
mean(z)


## 2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
max <- max(c(x, y))
slopes <- seq(0, 5, by=.001)
mse <- sapply(slopes, function(slope) {
    sum((y - x * slope) ^ 2)
})
slope <- slopes[which(mse==min(mse))]

plot(x, y, pch=20, xlim=c(0, max), ylim=c(0, max), main=slope)
abline(0, slope)


## 3
data(mtcars)
lm(mtcars$mpg ~ mtcars$wt)


## 6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
(x - mean(x)) / sd(x)

## 7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
x <- rnorm(40)
y <- rnorm(40, sd=2)
fit <- lm(y ~ x)
ic <- fit$coefficients[1]
slope <- fit$coefficients[2]
slope <- cov(x, y) / var(x)

plot(x, y, pch=20,
     main=paste("slope: ", round(slope,2), "\n",
                "intercept 0:", round(ic, 2)))
abline(fit)
abline(h=mean(y), v=mean(x), lty=3)
for(i in seq_along(fit$residuals)) {
    segments(x0=x[i], y0=x[i]*slope + ic, y1=y[i], lty=2)
}

# ## 9
# x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
# mean(x)