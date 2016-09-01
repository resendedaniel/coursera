#' Make nPlets
#'
#' @param d vector of tokenized words
#' @nplet the length of the DU from duplet
#' default is 2
#'
#' @return a matrix of nplets
#' @export
make_nplets <- function(d, nplet = 2) {
    # nplets didn't work as I expected. I'll let them here for now
    n <- length(d)
    matrix <- sapply(seq(n-1), function(i) {
        d[i:(i+1)]
    })
    matrix <- t(matrix)
    # matrix <- matrix[order(matrix[,1]),]
    return(matrix)
}

#' Tokenize data
#'
#' Remove al special characters and break it by word
#'
#' @param d a setence
#'
#' @return a list with words
#' @export
tokenizeData <- function(d) {
    cat(".")
    d <- gsub("[[:punct:]]", " ", d)
    d <- tolower(d)
    return(strsplit(d, "\\s+"))
}

#' loadData
#'
#' Get data from server or from cache
#'
#' @return The dataset
#'
#' @export
loadData <- function(n = -1L) {
    print("loading data")
    t <- proc.time()
    data_file <- getOption("capstone.data_file")
    if(!file.exists(data_file)) downloadData()
    data_dir <- paste("data", "final", getOption("capstone.lang"), sep="/")
    files <- list.files(data_dir)
    files_path <- list.files(data_dir, full.names = TRUE)
    data <- lapply(files_path,
                   readLines,
                   n = n,
                   warn = FALSE)
    names(data) <- files
    print(proc.time()) - t
    return(data)
}

#' downloadData
#'
#' Get data from server
#'
#' @return The dataset
#'
#' @export
downloadData <- function() {
    download.file(getOption("capstone.data_url"), getOption("capstone.data_file"))
    unzip(data_file, exdir = "data")
}
