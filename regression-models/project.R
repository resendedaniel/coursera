
with(mtcars, plot(am, mpg, pch=20))
abline(lm(mpg ~ am, mtcars), lty=2)

pairs(mtcars, pch=20)
cor(mtcars)

regressors <- names(mtcars)[-c(1, 9)]
formulas <- sapply(1:length(regressors), function(i) {
    currFormula <- "mpg ~ am "
    for(j in 1:i) {
        currFormula <- paste0(currFormula, " + ", regressors[j])
    }
    paste0(currFormula, "-1")
})
fit <- sapply(formulas, function(formula) {
    list(lm(formula, mtcars))
})

lapply(fit, function(f) summary(f)$sigma)
lapply(fit, function(f) summary(f)$r.squared)