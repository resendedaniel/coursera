library(reshape2)
library(ggplot2)
library(manipulate)
library(UsingR)
data(galton)

means <- sapply(galton, mean)
means <- data.frame(t(means))
# names(means) <- NULL
# means <- data.frame(variable=names(galton), value=means)

plot(galton$child, galton$parent, pch=19, col="blue")

ggplot(melt(galton), aes(value, fill=variable)) +
    geom_histogram(colour = "black", binwidth=1) +
    facet_grid(.~variable)

freqData <- as.data.frame(table(galton))
freqData <- sapply(freqData, function(x) {
    x <- as.numeric(as.character(x))
})
freqData <- as.data.frame(freqData)
names(freqData) <- c("child", "parent", "freq")

myPlot <- function(beta) {
    y0 <- means[[2]] - means[[1]] * beta
    y <- freqData$child * beta + y0
    mse <- sum((freqData$parent - y) ^ 2 * freqData$freq) / sum(freqData$freq)
    mse <- round(mse, 2)
    ggplot(filter(freqData, freq>0), aes(child, parent)) +
        geom_point(colour="blue", aes(size = freq)) +
        scale_size(range = c(2, 10), guide = "none" ) +
        geom_point(data=means, aes(child, parent)) + 
        geom_abline(intercept=y0, slope=beta) +
        ggtitle(paste("MSE:", mse, "\nbeta:", beta))
}
manipulate(myPlot(beta), beta=slider(-5, 5, initial=1, step=.1))

# x <- seq(min(galton$child), max(galton$child), by=.01)
# leastSquare <- sapply(x, function(i) {
# sum((galton$child - i) ^ 2)
# })
# plot(x, leastSquare)
# x[which(leastSquare==min(leastSquare))]
# mean(galton$child)
