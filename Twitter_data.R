
##################################
###  Extract twitter data  ###
##################################

library(twitteR)
library(RCurl)
library(ggmap)
library(data.table)

consumer_key <- "xGLLWIVOyd0QnlUNBy2oGTdww"
consumer_secret <- "r9i3tf4c51c6vSmDT3jSn8GQv1sdMe12MOehL0DCC6AtjFO8mZ"
access_token <- "3073357450-T4u5TN4whS9YOxNTrQMyBIuPQxFMEd87DVenn9A"
access_secret <- "Ke1ybbLvYp6P73iHNVzylnzKFQ4FUt99YpDXZodcaADeV"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

zillow_tweets <- searchTwitter("zillow",since='2016-09-01', until='2017-09-01', n=1000)
tweets.df <- twListToDF(zillow_tweets)

tweets.df[1:10,]

zillow_mentioners <- unique(tweets.df$screenName)

user_info <- twListToDF(lookupUsers(zillow_mentioners))

userTimeline(user_info$screenName[10], n=20)




##################################
###  map ###
##################################



library(stringi)
stri_enc_mark(tweets.df$location)

geo_codes <- geocode(stri_trans_general(tweets.df$location, "Latin-ASCII"))

dim(geo_codes)



train = data.frame(tweet_text = tweets.df$text,lon = geo_codes$lon, lat = geo_codes$lat, Time = tweets.df$created)
train = train[!is.na(train$lon),]
dim(train)


train
dim(train)
class(train$Time)
train$Time = as.character(train$Time)
train$Time = ymd_hms(train$Time)

train %>% filter(Time>=as.POSIXct("2016-12-01", tz = "UTC")) %>%
  leaflet() %>% addTiles() %>%
  addCircleMarkers(~lon, ~lat, 
                   radius = 0.1,
                   popup = ~tweet_text, fillOpacity = 0.5)




##################################
###  Sentiment ###
##################################

#tweets evaluation function
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

pos <- scan('data/positive-words.txt', what='character', comment.char=';') #folder with positive dictionary
neg <- scan('data/negative-words.txt', what='character', comment.char=';') #folder with negative dictionary
pos.words <- c(pos, 'upgrade')
neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')


scores <- score.sentiment(train$tweet_text, pos.words, neg.words, .progress='text')


summary(scores)
hist(scores$score)

scores$sentiment = "neutral"
scores$sentiment[scores$score>0] = "positive"
scores$sentiment[scores$score<0] = "negative"

train = data.frame(tweet_text = tweets.df$text,lon = geo_codes$lon, lat = geo_codes$lat, 
                   Time = tweets.df$created)
train = train[!is.na(train$lon),]
dim(train)

train= data.frame(train,score = scores$score,Category = scores$sentiment)

train = data.frame(train)

summary(train)

setwd("S:/Data Science Think Tank/Twitter_visual")
write.csv(train,"data/train.csv", row.names=FALSE)



train %>% filter(Time>=as.POSIXct("2016-12-01", tz = "UTC")) %>%
  filter(Category == c(unique(train$Category))) %>%
  leaflet() %>% addTiles() %>%
  addCircleMarkers(~lon, ~lat, 
                   radius = 0.1,
                   popup = ~tweet_text, fillOpacity = 0.5)





##################################
###  word cloud ###
##################################

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")


myCorpus =  Corpus(VectorSource(train$tweet_text))

myCorpus = tm_map(myCorpus, tolower)
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, removeWords, stopwords("english"))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
myCorpus <- tm_map(myCorpus, toSpace, "/")
myCorpus <- tm_map(myCorpus, toSpace, "@")
myCorpus <- tm_map(myCorpus, toSpace, "\\|")

myCorpus = tm_map(myCorpus, removeWords, "zillow")

myCorpus = tm_map(myCorpus,  stemDocument)


myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
m = as.matrix(myDTM)

v = sort(rowSums(m), decreasing = TRUE)

d <- data.frame(word = names(v),freq=v)
head(d)

d = d[!(is.na(match(d$word, pos.words))),]

head(d)
d=d[-1,]

set.seed(4363)

setwd("S:/Data Science Think Tank/Twitter_visual")
write.csv(d,"data/clouddata.csv", row.names=FALSE)

wordcloud(d$word, d$freq, 
          min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))



findFreqTerms(myDTM, lowfreq = 4)
findAssocs(myDTM, terms = "freedom", corlimit = 0.3)

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")




##################################
###  Social net ###
##################################



user <- zillow_mentioners[10]

realtor_christi <- getUser(user)
location(realtor_christi)

christi_followers <- realtor_christi$getFollowers(retryOnRateLimit=180)

christi_followers[1]

userTimeline(christi_followers[15], n=20)

christi_followers_df = rbindlist(lapply(christi_followers,as.data.frame))

head(christi_followers_df)




