data <-"https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
destfile <- "Capstone Swift Key Data.zip"
download.file(data, destfile)
unzip(destfile)
blogs <- "en_US.blogs.txt"
news <- "en_US.news.txt"
twitter <- "en_US.twitter.txt"



blog.line <- readLines(blogs, encoding = "UTF-8", skipNul = TRUE)
news.line <- readLines(news, encoding = "UTF-8", skipNul = TRUE)
twitter.line <- readLines(twitter, encoding = "UTF-8", skipNul = TRUE)

summary(nchar(blog.line))
summary(nchar(news.line))
summary(nchar(twitter.line))
library(magrittr)
library(stringi)
library(ggplot2)

blog.word.count <- stri_count_words(blog.line)
news.word.count <- stri_count_words(news.line)
twitter.word.count <- stri_count_words(twitter.line)

summary(blog.word.count)
str(blog.word.count)
length(blog.word.count)

head(blog.word.count)

length(twitter.word.count)
length(news.word.count)
str(twitter.word.count)

##Data Processing
blog.word <- unlist(strsplit(blog.line, " "))
news.word <- unlist(strsplit(news.line, " "))
twitter.word <- unlist(strsplit(twitter.line, " "))

##Finding punctuations, spaces, non-ASCII, and numbers
blog.blankspace <- sum(stri_count(blog.line, regex="\\p{Space}"))
news.blankspace <-sum(stri_count(news.line, regex="\\p{Space}"))
twitter.blankspace <- sum(stri_count(twitter.line, regex="\\p{Space}"))

blog.punc <- sum(stri_count(blog.line, regex="\\p{Punct}"))
news.punc <- sum(stri_count(news.line, regex="\\p{Punct}"))
twitter.punc <- sum(stri_count(twitter.line, regex="\\p{Punct}"))

blog.nonEnglish <- length(blog.word[stri_enc_isascii(unlist(blog.word))==FALSE])
news.nonEnglish <- length(news.word[stri_enc_isascii(unlist(news.word))==FALSE])
twitter.nonEnglish <- length(twitter.word[stri_enc_isascii(unlist(twitter.word))==FALSE])

blog.number <-length(blog.word[stri_detect_regex(blog.word, "[:digit:]")==TRUE])
news.number <-length(news.word[stri_detect_regex(news.word, "[:digit:]")==TRUE])
twitter.number <-length(twitter.word[stri_detect_regex(twitter.word, "[:digit:]")==TRUE])

summary(blog.number)
length(blog.number)
head(blog.number)

