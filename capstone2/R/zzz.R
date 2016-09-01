.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    capstone.data_file = "data/Coursera-SwiftKey.zip",
    capstone.data_url = "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
    capstone.lang = "en_US"
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])

  invisible()
}