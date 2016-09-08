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
    if (n < nplet) return()
    nplet <- nplet - 1
    matrix <- sapply(seq(n-nplet), function(i) {
        d[i:(i+nplet)]
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
loadData <- function(n = -1L, report = FALSE) {
    data_file <- getOption("capstone.data_file")
    if (report) data_file <- paste0("../", data_file)
    if(!file.exists(data_file)) downloadData()
    data_dir <- paste("data", "final", getOption("capstone.lang"), sep="/")
    if (report) data_dir <- paste0("../", data_dir)
    files <- list.files(data_dir)
    files_path <- list.files(data_dir, full.names = TRUE)
    data <- lapply(files_path,
                   readLines,
                   n = n,
                   warn = FALSE)
    sapply(files_path, close)
    names(data) <- files
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
