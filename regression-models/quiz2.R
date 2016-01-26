# Quiz 2

## 1
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y ~ x)
summary(fit)

## 3
data(mtcars)
fit <- lm(mpg ~ I(wt - mean(wt)), mtcars)
summary(fit)
with(mtcars, plot(wt - mean(wt), mpg))
abline(fit)
abline(h=mean(mtcars$mpg), v=0, lty=2)
sumCoef <- summary(fit)$coef
sumCoef[1,1] + c(-1,1) * qt(.975, df=fit$df) * sumCoef[2,2]


## 5
data(mtcars)
fit <- lm(mpg ~ wt, mtcars)
with(mtcars, plot(wt, mpg))
abline(fit)
predict(fit, data.frame(wt=mean(mtcars$wt)), interval = ("prediction"))
predict(fit, data.frame(wt=mean(mtcars$wt)), interval = ("confidence"))


## 6
data(mtcars)
x <- mtcars$wt / 2
x <- x - mean(x)
y <- mtcars$mpg
fit <- lm(y ~ x)
summary(fit)
predict(fit, interval = ("confidence"))

sumCoef <- summary(fit)$coef
sumCoef[2,1] + c(-1,1) * qt(.975, df=fit$df) * sumCoef[2,2]
