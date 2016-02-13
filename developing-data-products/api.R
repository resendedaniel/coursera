library(curl)
library(jsonlite)
library(dplyr)
library(reshape2)
library(lubridate)
library(ggplot2)

# Enviroment variables
PATH <- "https://staging_api.verios.com.br"
treasuries <- list()

#' Get asset daily series
#' 
#' Reach api and return a single asset daily series,
#' then clean it a little
#' 
#' @param asset_id [string]
#' @return [data.frame] with asset daily series
get_asset_daily <- function(asset_id) {
    if(asset_id %in% names(treasuries)) return(treasuries[[asset_id]])
    
    url <- paste0(PATH, "/assets/", asset_id, "/dailies?asc=d")
    
    ## Downloading data
    # message("Downloading ", url, "\n")
    
    data <- tryCatch({
        data <- fromJSON(url)
        
        ## Cleaning data
        data <- data %>%
            filter(!duplicated(d)) %>%
            filter(!is.na(j)) %>%
            mutate(d=from_mongo_date(d))
        
        ## Creating adjusted series if there isn't
        if (!"j" %in% names(data)) {
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
    
    treasuries[[asset_id]] <<- data
    
    return(data)
}

#' Put many assets together
#' 
#' Get each asset, bind them in a data.frame and filter by date.
#' The base asset is inflation (ipca) and all other assets are born from the inflation line
#' 
#' @param ids [string] assets ids
#' @param dates [Date] range of dates to filter data
#' 
#' @retur [data.frame] with all assets together
get_all_asset_dailies <- function(ids, dates=NULL) {
    ids <- c("ipca", ids)
    
    data <- lapply(ids, function(id) {
        daily <- get_asset_daily(id) %>%
            dplyr::select(d, j)
        names(daily)[2] <- ifelse(id == "ipca", "inflation", id)
        return(daily)
    })
    data <- Reduce(function(...) full_join(..., by ="d"), data)
    if (!is.null(dates)) data <- data %>% filter(d >= min(dates) & d <= max(dates))
    length <- as.numeric(apply(data[-1], 2, function(x) sum(!is.na(x))))
    oldest_ind <- which.max(length) + 1
    data[-1] <- data.frame(sapply(data[-1], function(x) {
        first <- min(which(!is.na(x)))
        ratio <- data[[oldest_ind]][first] / x[first]
        x <- x * ratio
        x
    }))
    
    return(data)
}

#' Get all treasuries ids
#' 
#' Reach api, get all assets and clean it
#' 
#' @return [string] assets ids array
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

#' Get maturity day
#' 
#' With the asset_id returns its maturity as Date
#' 
#' @param id [string] asset_id
#' @return [Date]
get_maturity_day <- function(id) {
    id <- sub("[a-z]+", "", id)
    maturity <- as.Date(id, "%Y%m%d")
    return(maturity)
}

#' Plot treasuries
#' 
#' Generate ggplot charts
#' 
#' @param data [data.frame] from get_all_asset_dailies
#' @return ggplot
plot_treasury <- function(data) {
    data <- melt(data, id="d") %>%
        filter(!is.na(value))
    
    ggplot(data, aes(d, value, colour=variable)) +
        geom_line() +
        scale_y_log10()
}

#' Parse mongo date
#' 
#' @param date [string]
#' @return [Date]
from_mongo_date <- function(date) {
    return(as.Date(sub("T.*", "", date)))
}