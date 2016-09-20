# load("data/processed.rda")
library(dplyr)
library(ggplot2)

predict <- function(pattern, plot_top = FALSE) {
    x <- ngram_freq$ngrams[grep(pattern, ngram_freq$ngrams)]
    model <- ".(_)"
    regex_pattern <- gsub("_", pattern, model)
    x <- gsub(regex_pattern, "", x)
    x <- sub(pattern, "", x)
    x <- sapply(x, function(each_x) {
        if(substr(each_x, 1, 1) == " ") each_x <- substr(each_x, 2, nchar(each_x))
        # if(substr(each_x, nchar(each_x), nchar(each_x)) == " ") each_x <- substr(each_x, 1, nchar(each_x) - 1)
        each_x
    })
    x <- x[x != ""]
    x <- data.frame(x, count = 1, stringsAsFactors = FALSE)
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

    if (plot_top) {
        g <- ggplot(x[1:(min(nrow(x), 10)), ], aes(factor(ngrams, rev(ngrams)), prop)) +
            geom_bar(stat="identity") +
            scale_y_continuous(labels = scales::percent) +
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank()) +
            coord_flip() #+ scale_x_reverse() # It should work in my world
        plot(g)
    }

    x$ngrams[1]
}
predict("who is", TRUE)
