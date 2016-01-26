require(datasets)
library(stats)
library(ggplot2)
library(GGally)
library(gridExtra)

data(swiss)
# ggpairs(swiss, lower=list(continuous="smooth"), params=c(method="loess"))

n <- 100
x2 <- 1:n
x1 <- .01 * x2 + runif(n, -.1, .1)
y <- -x1 + x2 + rnorm(n, sd=.01)

data <- data.frame(y, x1, x2, ey=resid(lm(y ~ x2)), ex1=resid(lm(x1 ~ x2)))

fit1 <-lm(y ~ x1)
fit2 <-lm(y ~ x1 + x2)
summary(fit1)$coef
summary(fit2)$coef

par(mfrow=c(2,1))
## 1
plot(x1, y, pch=20)
abline(fit1)
## 2
plot(x2, y, pch=20)
fit2 <-lm(y ~ x1 + x2)

g1 <- ggplot(data, aes(y=y, x=x1, colour=x2)) +
    geom_point(size=5) +
    geom_smooth(method="lm", se=FALSE, colour="red")
g2 <- ggplot(data, aes(y=ey, x=ex1, colour=x2)) +
    geom_point(size=5) +
    geom_smooth(method="lm", sd=FALSE, colour="red")
grid.arrange(g1, g2, ncol=1)


data(InsectSprays)
ggplot(InsectSprays, aes(y=count, x=spray, fill=spray)) +
    geom_violin(colour="black", size=2) +
    xlab("Type of spray") + ylab("Insect count")
 
summary(lm(count ~ I(1 * (spray=="B")) +
                   I(1 * (spray=="C")) +
                   I(1 * (spray=="D")) +
                   I(1 * (spray=="E")) +
                   I(1 * (spray=="F")), data=InsectSprays))$coef

spray2 <- relevel(InsectSprays$spray, "C")
summary(lm(count ~ spray2, data=InsectSprays))$coef