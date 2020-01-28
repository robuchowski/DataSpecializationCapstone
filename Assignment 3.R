library(stringi)
library(stringr)
library(tm)
library(wordcloud2)
library(ggplot2)
install.packages("wordcloud")
library(wordcloud)
conb <- file("~/Yale Papers/Data Analysis/Data Science Capstone/final/en_US/en_US.blogs.txt", "r")
conn <- file("~/Yale Papers/Data Analysis/Data Science Capstone/final/en_US/en_US.news.txt", "r")
connt <-file("~/Yale Papers/Data Analysis/Data Science Capstone/final/en_US/en_US.twitter.txt", "r")
setwd("~/Yale Papers/Data Analysis/Data Science Capstone/final/en_US")

blogs2 <- readLines(conb, 1000)
news2 <-readLines(conn, 1000)
twitter2 <- readLines(connt, 1000)
sData2 <-c(blogs2, news2, twitter2)
dcorpus <-Corpus(VectorSource(sData2))
corpus2 <-VCorpus(VectorSource(sData2), readerControl=list(readPlain, language="en", load=TRUE))

close(conb)
close(conn)
close(connt)

tokenmaker <- function(x) {
        corpus2 <-tm_map(corpus2, removePunctuation)
corpus2<- tm_map(corpus2, removeNumbers)
corpus2<- tm_map(corpus2, stripWhitespace)
corpus2<- tm_map(corpus2, removeWords, stopwords("english"))
corpus2 <- tm_map(corpus2, content_transformer(tolower))
corpus2 <- tm_map(corpus2, PlainTextDocument)
corpus2<- tm_map(corpus2, stemDocument)
corpus2 <- Corpus(VectorSource(corpus))
}

wordCounter <- function(x) {
        dtm <- DocumentTermMatrix(x)
        dtm_matrix <- as.matrix(dtm)
        word_freq <- colSums(dtm_matrix)
        word_freq<- sort(word_freq, decreasing = TRUE)
        words <- names(word_freq)
        return(list(words, word_freq))
}

NextWordIs <- function(x,y){
        BQuest <-grepl(x, blogs2, ignore.case = TRUE)
        BDocs <- blogs2[BQuest]
        textto <-'a'
        NextWordIs<-'a'
        i <-length(BDocs)
        if(i>0)
        {for (i in 1:i)
        {textto[i]<- str_extract(BDocs[i],y)
                NextWordIs[i]<- stri_extract_last_words(textto[i])}}
        NQuest <- grepl(x, news2, ignore.case = TRUE)
        NDocs <-news2[NQuest]
        j=length(NDocs)
        if (j>0){
                for (j in 1:j)
                {textto[i+j] <- str_extract(NDocs[j],y)
                NextWordIs[i+j]<- stri_extract_last_words(textto[i+j])}
        }
        TQuest<-grepl(x, twitter2, ignore.case = TRUE)
        TDocs <- twitter2[TQuest]
        k=length(TDocs)
        if(k>0)
        { for(k in 1:k)
        {textto[i+j+k]<- str_extract(TDocs[k],y)
                NextWordIs[i+j+k]<- stri_extract_last_words(textto[i+j+k])}}
        bundle<-as.data.frame(NextWordIs, stringAsFactors=FALSE)
        summary(bundle)
        blogs_token <- tokenmaker(budle)
        blogs_words <- wordCounter(blogs_token)
        summary(nchar(bundle))
        head(bundle)
        tdm_Blogs <- TermDocumentMatrix((blogs_token))
        m_Blogs <- as.matrix(tdm_Blogs)
        v_Blogs<- sort(rowSums(m_Blogs), decreasing = TRUE)
        d_Blogs<- data.frame(word=names(v_Blogs), freq=v_Blogs)
        head(v_Blogs, 100)
        return(list(head(v_Blogs, 100)))
}
result_1<-NextWordIs("a case of ", "([Aa]+ +[Cc]ase+ +[Oo]f+ +[^ ]+ )" )
