library(jsonlite)
library(dplyr)
library(reshape2)
library(lubridate)

PATH <- "https://api.verios.com.br"

get_asset_daily <- function(asset_id, period=NULL) {
    if(asset_id %in% names(treasuries)) return(treasuries[[asset_id]])
    
    url <- paste0(PATH, "/assets/", asset_id, "/dailies?asc=d")
    
    ## Downloading data
    # message("Downloading ", url, "\n")
    
    data <- tryCatch({
        data <- fromJSON(url)
        
        ## Cleaning data
        data <- data %>%
            filter(!duplicated(d)) %>%
            mutate(d=from_mongo_date(d))
        
        ## Creating adjusted series if there isn't
        if(!"j" %in% names(data)) {
            data <- data %>% mutate(j = v)
        }
        
        dailies_cache <- options()$dailies_cache
        dailies_cache[[asset_id]] <- data
        options(dailies_cache = dailies_cache)
        
        data
    }, error=function(cond) {
        message("No data available:")
        message(cond)
        return(NA)
    })
    
    if(!is.null(period)) {
        data <- filter(data, d>=period$start, d<=period$end)
    }
    
    treasuries[[asset_id]] <<- data
    
    return(data)
}


get_treasuries_id <- function() {
    print("Getting treasuries")
    
    url <- paste0(PATH, "/assets")
    ids <- fromJSON(url)[["_id"]]
    type <- c("^ntnb", "^ltn", "^lft", "^ntnf", "^ntnc")
    valid <- sapply(type, function(pattern) {
        grepl(pattern, ids)
    })
    valid <- apply(valid, 1, any)
    ids <- ids[valid]
    ids <- ids[!grepl("ideal", ids)]

    return(ids)
}

get_maturity_day <- function(id) {
    id <- sub("[a-z]+", "", id)
    maturity <- as.Date(id, "%Y%m%d")
    return(maturity)
}

is_matured <- function(date) {
    date < today()
}

plot_treasury <- function(ids) {
    ids <- c("ipca", ids)

    data <- lapply(ids, function(id) {
        daily <- get_asset_daily(id) %>%
            dplyr::select(d, j)
        names(daily)[2] <- id
        return(daily)
    })
    data <- Reduce(function(...) full_join(..., by ="d"), data)
    length <- apply(data[-1], 2, function(x) sum(!is.na(x)))
    oldest_ind <- which.max(length) + 1
    data[-1] <- apply(data[-1], 2, function(x) {
        first <- min(which(!is.na(x)))
        ratio <- data[first, oldest_ind] / x[first]
        x <- x * ratio
        return(x)
    })
    
    data <- melt(data, id="d")
    
    ggplot(data, aes(d, value, colour=variable)) +
        geom_line()
}

from_mongo_date <- function(date) {
    return(as.Date(sub("T.*", "", date)))
}