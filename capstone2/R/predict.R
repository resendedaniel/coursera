predict <- function(pattern) {
    x <- ngram_freq$ngrams[grep(pattern, ngram_freq$ngrams)]
    model <- ".(_)"
    l <- gsub("_", pattern, model)
    x <- gsub(l, "", x)
    x <- sub(pattern, "", x)
    x <- sapply(x, function(each_x) {
        if(substr(each_x, 1, 1) == " ") each_x <- substr(each_x, 2, nchar(each_x))
        # if(substr(each_x, nchar(each_x), nchar(each_x)) == " ") each_x <- substr(each_x, 1, nchar(each_x) - 1)
        each_x
    })
    x <- x[x != ""]
    x <- data.frame(x, count = 1)
    x <- aggregate(count ~ x, x, sum) %>%
        mutate(prop = count / sum(count)) %>%
        rename(ngrams = x) %>%
        dplyr::select(ngrams, prop)

    valid_ngram <- ngram_freq %>%
        filter(ngrams %in% unique(x$ngrams)) %>%
        dplyr::select(ngrams, prop)
    missing_ngram <- x %>%
        filter(!ngrams %in% valid_ngram$ngrams) %>%
        mutate(prop = prop ^2)
    x <- do.call(rbind, list(x, valid_ngram, missing_ngram))
    x <- aggregate(prop ~ ngrams, x, prod) %>%
        mutate(prop = prop / sum(prop)) %>%
        arrange(-prop)

    x
}
predict("i would")
