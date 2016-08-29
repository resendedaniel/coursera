library(capstone2)

raw_data <- loadData(n = 10000)
# data <- data[20]
data <- sapply(raw_data, tokenizeData, USE.NAMES=FALSE)
duplets <- sapply(data, make_nplets)
duplets <- do.call(rbind, duplets)
duplets_by_word <- split(duplets, duplets[,1])
x <- sapply(duplets_by_word, table)

