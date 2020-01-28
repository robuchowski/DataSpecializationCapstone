
library(NLP)
library(tm)
library(ggplot2)
library(ngram)
library(NLP)
library(tm)
library(magrittr)
library(stringi)
library(slam)
library(xtable)
library(RWeka)
library(wordcloud2)
blog.line <- readLines(blogs, encoding = "UTF-8", skipNul = TRUE)
news.line <- readLines(news, encoding = "UTF-8", skipNul = TRUE)
twitter.line <- readLines(twitter, encoding = "UTF-8", skipNul = TRUE)

blog.word.count <- stri_count_words(blog.line)
news.word.count <- stri_count_words(news.line)
twitter.word.count <- stri_count_words(twitter.line)

length(blog.word.count)
length(twitter.word.count)
length(news.word.count)

sBlog <- readLines("en_US.blogs.txt", 1000)
sNews <- readLines("en_US.news.txt", 1000)
sTwitter <- readLines("en_US.twitter.txt", 1000)
sData <-c(sBlog, sNews, sTwitter)
dcorpus <-Corpus(VectorSource(sData))

#Cleaning and explorartory
dcorpus <-tm_map(dcorpus, removePunctuation)
dcorpus <- tm_map(dcorpus, removeNumbers)
dcorpus <- tm_map(dcorpus, stripWhitespace)
dcorpus <- tm_map(dcorpus, removeWords, stopwords("english"))
dcorpus <- tm_map(dcorpus, content_transformer(tolower))

#term document matrix
tdm = TermDocumentMatrix(dcorpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq=v)
## New Way of doing it 
conb <- file("~/Yale Papers/Data Analysis/Data Science Capstone/final/en_US/en_US.blogs.txt", "r")
conn <- file("~/Yale Papers/Data Analysis/Data Science Capstone/final/en_US/en_US.news.txt", "r")
connt <-file("~/Yale Papers/Data Analysis/Data Science Capstone/final/en_US/en_US.twitter.txt", "r")

blogs2 <- readLines(conb, 1000)
news2 <-readLines(conn, 1000)
twitter2 <- readLines(connt, 1000)
sData2 <-c(blogs2, news2, twitter2)
dcorpus <-Corpus(VectorSource(sData2))
corpus2 <-VCorpus(VectorSource(sData2), readerControl=list(readPlain, language="en", load=TRUE))

close(conb)
close(conn)
close(connt)

corpus2 <-tm_map(corpus2, removePunctuation)
corpus2<- tm_map(corpus2, removeNumbers)
corpus2<- tm_map(corpus2, stripWhitespace)
corpus2<- tm_map(corpus2, removeWords, stopwords("english"))
corpus2 <- tm_map(corpus2, content_transformer(tolower))

# gram 1
gram1 = as.data.frame((as.matrix(TermDocumentMatrix(corpus2))))
gram1v <- sort(rowSums(gram1), decreasing = TRUE)
gram1d <- data.frame(word= names(gram1v), freq=gram1v)
gram1d[1:10,]
#gram 2
ggplot(gram1d[1:30,], aes(x=reorder(word,freq), y=freq)) +
        geom_bar(stat="identity", width=0.5, fill= "blue") +
        labs(title= "Unigrams") +
        xlab("Unigrams") +ylab("Frequency") +
        theme(axis.text.x = element_text(angle=65, vjust = 0.6))

bigram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
gram2= as.data.frame((as.matrix(TermDocumentMatrix(corpus2, control = list(tokenize=bigram)))))
gram2v <- sort(rowSums(gram2), decreasing = TRUE)
gram2d <- data.frame(word = names(gram2v), freq=gram2v)
gram2d[1:10,]
#3 gram
trigram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max=3))
gram3= as.data.frame((as.matrix(TermDocumentMatrix(corpus2, control = list(tokenize = trigram)))))
gram3v <- sort(rowSums(gram3), decreasing = TRUE)
gram3d <- data.frame(word = names(gram3v), greq=gram3v)
gram3d[1:10,]

# histogram
ggplot(gram1d[1:30,], aes(x=reorder(word,freq), y=freq)) +
        geom_bar(stat="identity", width=0.5, fill= "blue") +
        labs(title= "Unigrams") +
        xlab("Unigrams") +ylab("Frequency") +
        theme(axis.text.x = element_text(angle=65, vjust = 0.6))
