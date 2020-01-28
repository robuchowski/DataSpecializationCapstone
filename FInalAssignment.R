library(tm)
library(dplyr)
library(stringi)
library(stringr)
library(quanteda)
library(data.table)

rm(list = ls(all.names = TRUE))


con1<- file("~/Yale Papers/Data Analysis/Data Science Capstone/final/en_US/en_US.blogs.txt", "rb")
con2 <- file("~/Yale Papers/Data Analysis/Data Science Capstone/final/en_US/en_US.news.txt", "rb")
con3 <-file("~/Yale Papers/Data Analysis/Data Science Capstone/final/en_US/en_US.twitter.txt", "rb")

blogs <- readLines(con3, encoding = "UTF-8", skipNul = TRUE)
news <- readLines(con2, encoding = "UTF-8", skipNul = TRUE)
twitter <- readLines(con1, encoding = "UTF-8", skipNul = TRUE)

close(con1)
close(con2)
close(con3)

print("Loaded training data")
print(paste0("Number of lines per file (blogs):     ", format(length(blogs), big.mark = ",")))
print(paste0("Number of lines per file (news):    ", format(length(news), big.mark = ",")))
print(paste0("Number of lines per file (twitter): ", format(length(twitter), big.mark = ",")))
print(paste0("Number of lines per file (total):   ", format(length(blogs) +
                                                                    length(news) +
                                                                    length(twitter), big.mark = ",")))                                                               

rm(con, trainURL, trainDataFile, blogsFileName, newsFileName, twitterFileName)
set.seed(4114)

sampleSize = 0.05

# sample all three data sets
sampleBlogs <- sample(blogs, length(blogs) * sampleSize, replace = FALSE)
sampleNews <- sample(news, length(news) * sampleSize, replace = FALSE)
sampleTwitter <- sample(twitter, length(twitter) * sampleSize, replace = FALSE)

# remove all non-English characters from the sampled data
sampleBlogs <- iconv(sampleBlogs, "latin1", "ASCII", sub = "")
sampleNews <- iconv(sampleNews, "latin1", "ASCII", sub = "")
sampleTwitter <- iconv(sampleTwitter, "latin1", "ASCII", sub = "")

removeOutliers <- function(data) {
        first <- quantile(nchar(data), 0.25)
        third <- quantile(nchar(data), 0.75)
        data <- data[nchar(data) > first]
        data <- data[nchar(data) < third]
        return(data)
}

sampleBlogs <- removeOutliers(sampleBlogs)
sampleNews <- removeOutliers(sampleNews)
sampleTwitter <- removeOutliers(sampleTwitter)

# combine all three data sets into a single data set
sampleData <- c(sampleBlogs, sampleNews, sampleTwitter)

# get number of lines and words from the sample data set
sampleDataLines <- length(sampleData)
sampleDataWords <- sum(stri_count_words(sampleData))
print("Create sample data set")
print(paste0("Number of lines:  ", format(sampleDataLines, big.mark = ",")))
print(paste0("Number of words: ", format(sampleDataWords, big.mark = ",")))

# remove variables no longer needed to free up memory
rm(blogs, news, twitter, sampleBlogs, sampleNews, sampleTwitter)
rm(removeOutliers)

# load bad words file
badWordsURL <- "http://www.idevelopment.info/data/DataScience/uploads/full-list-of-bad-words_text-file_2018_07_30.zip"
badWordsFile <- "full-list-of-bad-words_text-file_2018_07_30.txt"

download.file(badWordsURL, badWordsFile)
if (!file.exists(badWordsFile)) {
        tempFile <- tempfile()
        download.file(badWordsURL, tempFile)
        unzip(tempFile)
        unlink(tempFile)
        rm(tempFile)
}
con <- file(badWordsFile, open = "rb")
profanity <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
profanity <- iconv(profanity, "latin1", "ASCII", sub = "")
close(con)

sampleData <- tolower(sampleData)

sampleData <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", sampleData, ignore.case = FALSE, perl = TRUE)
sampleData <- gsub("\\S+[@]\\S+", "", sampleData, ignore.case = FALSE, perl = TRUE)
sampleData <- gsub("@[^\\s]+", "", sampleData, ignore.case = FALSE, perl = TRUE)
sampleData <- gsub("#[^\\s]+", "", sampleData, ignore.case = FALSE, perl = TRUE)

# remove ordinal numbers
sampleData <- gsub("[0-9](?:st|nd|rd|th)", "", sampleData, ignore.case = FALSE, perl = TRUE)

# remove punctuation
sampleData <- gsub("[^\\p{L}'\\s]+", "", sampleData, ignore.case = FALSE, perl = TRUE)

# remove punctuation (leaving ')
sampleData <- gsub("[.\\-!]", " ", sampleData, ignore.case = FALSE, perl = TRUE)

# trim leading and trailing whitespace
sampleData <- gsub("^\\s+|\\s+$", "", sampleData)
sampleData <- stripWhitespace(sampleData)

# write sample data set to disk
sampleDataFileName <- "en_US.sample.txt"
con <- file(sampleDataFileName, open = "w")
writeLines(sampleData, con)
close(con)

# remove variables no longer needed to free up memory
rm(badWordsURL, badWordsFile, con, sampleDataFileName, profanity)

corpus <- corpus(sampleData)

getTopThree <- function(corpus) {
        first <- !duplicated(corpus$token)
        balance <- corpus[!first,]
        first <- corpus[first,]
        second <- !duplicated(balance$token)
        balance2 <- balance[!second,]
        second <- balance[second,]
        third <- !duplicated(balance2$token)
        third <- balance2[third,]
        return(rbind(first, second, third))
}

tokenFrequency <- function(corpus, n = 1, rem_stopw = NULL) {
        corpus <- dfm(corpus, ngrams = n)
        corpus <- colSums(corpus)
        total <- sum(corpus)
        corpus <- data.frame(names(corpus),
                             corpus,
                             row.names = NULL,
                             check.rows = FALSE,
                             check.names = FALSE,
                             stringsAsFactors = FALSE
        )
        colnames(corpus) <- c("token", "n")
        corpus <- mutate(corpus, token = gsub("_", " ", token))
        corpus <- mutate(corpus, percent = corpus$n / total)
        if (n > 1) {
                corpus$outcome <- word(corpus$token, -1)
                corpus$token <- word(string = corpus$token, start = 1, end = n - 1, sep = fixed(" "))
        }
        setorder(corpus, -n)
        corpus <- getTopThree(corpus)
        return(corpus)
}

# get top 3 words to initiate the next word prediction app
startWord <- word(corpus$documents$texts, 1)  # get first word for each document
startWord <- tokenFrequency(startWord, n = 1, NULL)  # determine most popular start words
startWordPrediction <- startWord$token[1:3]  # select top 3 words to start word prediction app
saveRDS(startWordPrediction, "start-word-prediction2.RData")

# bigram
bigram <- tokenFrequency(corpus, n = 2, NULL)
write.csv(bigram, "bigram3.csv")
bigram <- read.csv("bigram3.csv", stringsAsFactors = F)
saveRDS( bigram, "bigram.RData")

# trigram
trigram <- tokenFrequency(corpus, n = 3, NULL)
trigram <- trigram %>% filter(n > 1)
write.csv(trigram, "trigram.csv")
trigram <-read.csv("trigram.csv", stringsAsFactors = F)
saveRDS(trigram, "trigram.RData")
# quadgram
quadgram <- tokenFrequency(corpus, n = 4, NULL)
quadgram <- quadgram %>% filter(n > 1)
write.csv(quadgram, "quadgram.csv")
quadgram <- read.csv("quadgram.csv", stringsAsFactors = F)
saveRDS(quadgram, "quadram4.RData")

