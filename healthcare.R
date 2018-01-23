library(twitteR)
library(ROAuth)
library(httr)
library (ggplot2)
library(plyr)
library(stringr)
library(tm)
library(wordcloud)

#API Keys go to https://apps.twitter.com/ to acess
api_key <- "Jcow5IlU6pcshEKFXE1rBFNh5"
api_secret <- "yW4qdL3BhSUpu60o4ddn4IbpAOz84prUbdxZT9YknR4hFyaiHE"
access_token <- "777929621215981568-K9lzZYmfdp7u8EVBsKgcpRENmtadqhl"
access_token_secret <- "6apQ4EbLlzLJ4uVuDFfCdQH05OWNjithUN5vgBTdEOa5g"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# Time to grab the latest tweets and determine the amount you want
tweets_healthcare <- searchTwitter('#healthcare', n=1500)

txt <- sapply(tweets_healthcare, function(x) x$getText()) #extracts text only

#clean the tweets
txt <- iconv(txt, "latin1", "ASCII", sub = "")
#get rid of any links
txt <- gsub("http(s?)([^ ]*)", " ", txt, ignore.case = T) 
#more cleaning removing the html and &amp
txt <- gsub("&amp", "and", txt) 

#create a corpus
corp <- Corpus(VectorSource(txt)) 

#create custom stopwords to get a better picture
moreStopWords <- c("might", "put", "via", "see", "join", 
                   "jobs", "apply", "know", "one", "will", "like",
                   "use", "don","get","like" "didnt", "cant") 

# Create a term document matrix and use stopwords
ctrl <- list(removePunctuation= list(preserve_intra_word_dashes = T),
             tolower= T,
             stopwords= c(stopwords(kind = "en"), moreStopWords),
             removeNumbers= T)
tdm <- TermDocumentMatrix(x= corp, control= ctrl)

findFreqTerms(tdm, 20)

wrdFreqs <- sort(rowSums(as.matrix(tdm)), decreasing= T)
wrdFreqsDF <- data.frame(word= names(wrdFreqs), 
                         freq= wrdFreqs, 
                         stringsAsFactors= F, row.names= NULL)

wordcloud(words= wrdFreqsDF$word, freq= wrdFreqsDF$freq, scale = c(3, 0.5), 
          random.order= F, min.freq= 15, colors= brewer.pal(8, "Set1"))


tweets_op <- searchTwitter('#healthcare', n=1500)

txt <- sapply(tweets_healthcare, function(x) x$getText()) #extracts text only

#clean the tweets
txt <- iconv(txt, "latin1", "ASCII", sub = "")
#get rid of any links
txt <- gsub("http(s?)([^ ]*)", " ", txt, ignore.case = T) 
#more cleaning removing the html and &amp
txt <- gsub("&amp", "and", txt) 

#create a corpus
corp <- Corpus(VectorSource(txt)) 

#create custom stopwords to get a better picture
moreStopWords <- c("might", "put", "via", "see", "join", 
                   "jobs", "apply", "know", "one", "will", "like",
                   "use", "don","get","like" "didnt", "cant") 

# Create a term document matrix and use stopwords
ctrl <- list(removePunctuation= list(preserve_intra_word_dashes = T),
             tolower= T,
             stopwords= c(stopwords(kind = "en"), moreStopWords),
             removeNumbers= T)
tdm <- TermDocumentMatrix(x= corp, control= ctrl)

findFreqTerms(tdm, 20)

wrdFreqs <- sort(rowSums(as.matrix(tdm)), decreasing= T)
wrdFreqsDF <- data.frame(word= names(wrdFreqs), 
                         freq= wrdFreqs, 
                         stringsAsFactors= F, row.names= NULL)
term.freq = rowSums(as.matrix(tdm))
term.freq = subset(term.freq, term.freq >= 50)
df=data.frame(term=names(term.freq), freq=term.freq, freq = term.freq)

ggplot(df, aes(x=term, y=freq)) +geom_bar(stat="identity") +
 xlab("Terms") + ylab("Count") + coord_flip() +
 theme(axis.text = element_text(size=7))

wordcloud(words= wrdFreqsDF$word, freq= wrdFreqsDF$freq, scale = c(3, 0.5), 
          random.order= F, min.freq= 15, colors= brewer.pal(8, "Set1"))

#find the associations
findAssocs(tdm, "cancer", 0.2)

findAssocs(tdm, "opioid", 0.2)

findAssocs(tdm, "overdose", 0.2)

debt=findAssocs(tdm, "debt", 0.2)
head(debt)
findAssocs(tdm, "trump", 0.2)

findAssocs(tdm, "obamacare", 0.2)

findAssocs(tdm, "hospital", 0.2)

library(topicmodels)
dtm=as.DocumentTermMatrix(tdm)
#8 topics
lda=LDA(dtm, k=8)
term=terms(lda,7)
(term=lapply(term, MARGIN=2, paste, collapse=", "))
findAssocs(tdm, "data", 0.2)
topics=topics(lda)
topics=data.frame(date=as.IDate(tweets.df$created), topic=topics)
ggplot(topics, aes(date, fill=term[topic]))+
 geom_density(position = 'stack')
