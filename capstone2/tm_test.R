library(tm)
library(ngram)
library(R.utils)
library(dplyr)
library(ggplot2)

create_corpus <- function(vector_text_data){
    data_vector <- VectorSource(vector_text_data)
    corpus <- Corpus(data_vector)

    return(corpus)}

create_dtm <- function(corpus){
    dtm <- DocumentTermMatrix(corpus)
    return(dtm)}

clean_data <- function(data){
    data <- tolower(data)
    ##Multiple occurences of "?", "!" and "."
    data <- gsub("\\.{2,}", "\\.", data)
    data <- gsub("!{2,}", "!", data)
    data <- gsub("\\?{2,}", "?", data)
    ##Clean up apostrophes ('re; 's)
    data <- gsub("'re", " are", data)
    data <- gsub("'s", " is", data)
    ##Remove URLs
    data <- gsub('http\\S+\\s*', '', data)
    return(data)
}

sentence_lengths <- function(data_clean){
    data_split <- data_clean %>%
        stri_split(regex = "\\.|\\?|!") %>%
        unlist()

    empty_idx <- data_split == ""
    data_split <- data_split[!empty_idx]

    sentence_length <- numeric()
    for(i in 1:length(data_split)){
        sentence_length[i] <- length(unlist(strsplit(data_split[i], " ")))
    }
    return(sentence_length)
}

ngram_freq <- function(data_vector){
    data_2grams <- ngram(data_vector, n = 2)
    data_3grams <- ngram(data_vector, n = 3)
    data_4grams <- ngram(data_vector, n = 4)
    data_2gram_freq <- get.phrasetable(data_2grams) %>% cbind(2)
    names(data_2gram_freq)[4] <- "n"
    data_3gram_freq <- get.phrasetable(data_3grams) %>% cbind(3)
    names(data_3gram_freq)[4] <- "n"
    data_4gram_freq <- get.phrasetable(data_4grams) %>% cbind(4)
    names(data_4gram_freq)[4] <- "n"
    ngram_freq_df <- rbind(data_2gram_freq, data_3gram_freq, data_4gram_freq) %>%
        arrange(-freq) %>%
        mutate(prop = freq / sum(freq))
    return(ngram_freq_df)
}

coverage <- function(word_freqs){
    data_coverage_nWords <- cumsum(word_freqs$frequency)/sum(word_freqs$frequency)
    data_coverage_df <- data.table(n_words = seq(1:length(data_coverage_nWords)),
                                   coverage = data_coverage_nWords)
    return(data_coverage_df)
}

readData <- function(p = .1) {
    t0 <- proc.time()

    file <- "data/final/en_US/en_US.twitter.txt"
    n_lines <- countLines(file)[[1]]
    twitter <- readLines(file, n = round(n_lines * p),encoding = "UTF-8")
    twitter <- sample(twitter, round(length(twitter) * p))

    file <- "data/final/en_US/en_US.news.txt"
    n_lines <- countLines(file)[[1]]
    news <- readLines(file, n = round(n_lines * p),encoding = "UTF-8")
    news <- sample(news, round(length(news) * p))

    file <- "data/final/en_US/en_US.blogs.txt"
    n_lines <- countLines(file)[[1]]
    blogs <- readLines(file, n = round(n_lines * p),encoding = "UTF-8")
    blogs <- sample(blogs, round(length(blogs) * p))

    data <- c(twitter, news, blogs)

    t1 <- proc.time()
    print(t1 - t0)
    cat("Read", round(length(data) / as.numeric(t1 - t0)[3]), "lines/s", "\n")

    return(data)
}

#### Script ####

data <- readData(p = .25)

t0 <- proc.time()
#Create character vectors
n <- length(data)
data <- clean_data(data)
data_vector <- paste(data, collapse = " ")

#Create corpora and DTMs
corpora <- create_corpus(data)
dtm <- create_dtm(corpora)
ngram_freq <- ngram_freq(data_vector)

t1 <- proc.time()
print(t1 - t0)
cat("Processed", round(n / as.numeric(t1 - t0)[3]), "lines/s", "\n")

slope <- 4
ngram_freq <- ngram_freq %>%
    filter(prop >= slope / nrow(.))

# g <- ggplot(ngram_freq, aes(x=seq(length(freq))/length(freq),
#                             y=cumsum(freq)/sum(freq))) +
#     geom_line()
# print(g)
