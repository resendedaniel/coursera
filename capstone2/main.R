library(capstone2)
library(dplyr)
library(ggplot2)
library(scales)

raw_data <- loadData(n = 200)
# data <- data[20]
data <- sapply(raw_data, tokenizeData, USE.NAMES=FALSE)
count_data <- data.frame(token = unlist(data)) %>%
    mutate(count = 1)
count_data <- aggregate(count ~ token, count_data, sum) %>%
    mutate(p = count / sum(count)) %>%
    arrange(-p) %>%
    mutate(cump = cumsum(p)) %>%
    mutate(universe = seq(nrow(.)) / nrow(.))

g_token <- ggplot(count_data, aes(universe, cump)) +
    geom_line() +
    scale_y_continuous("cumulate occurence", labels = percent) +
    scale_x_continuous("token", labels = percent)

duplets <- sapply(data, make_nplets)
duplets <- do.call(rbind, duplets)
duplets_by_word <- split(duplets[,-1], duplets[,1])
x <- sapply(duplets_by_word, function(x) rev(sort(table(x))))

