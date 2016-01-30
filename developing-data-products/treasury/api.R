library(jsonlite)
library(dplyr)
library(reshape2)

getTreasuriesIDs <- function() {
    # Get all treasuries with some cleaning args
    #
    # Args:
    #   principal: if FALSE remove ntnbprincipal
    #   ideal: if FALSE remove ideal series
    #
    # Returns:
    #   vector of valid assets id
    path <- "https://api.verios.com.br"
    path <- paste0(path, "/assets")
    ids <- fromJSON(path)[["_id"]]
    type <- c("^ntnb", "^ltn", "^lft", "^ntnf", "^ntnc")
    valid <- sapply(type, function(pattern) {
        grepl(pattern, ids)
    })
    valid <- apply(valid, 1, any)
    ids <- ids[valid]
    ids <- ids[!grepl("ideal", ids)]
    
    return(ids)
}

plot_treasury <- function(id) {
    path <- "https://api.verios.com.br"
    path <- paste0(path, "/assets/{id}/dailies")
    path <- sub("\\{id\\}", id, path)
    data <- fromJSON(path) %>%
        mutate(d = as.Date(sub("T.*", "", d)))
    ggplot(data, aes(d, j)) + geom_line() +
        ggtitle(id)
    
}
