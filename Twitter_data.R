
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

require(devtools)
install_github("lchiffon/wordcloud2")

library(wordcloud2)
wordcloud2(data = d)

letterCloud(d, word = "zillow", wordSize = 1)


dim(d)
dim(demoFreq)

findFreqTerms(myDTM, lowfreq = 4)
findAssocs(myDTM, terms = "freedom", corlimit = 0.3)

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")



##################################
###  Time HeatMaps ###
##################################

names(train)
summary(train$Time)
class(train$Time)

library(lubridate)
train$weekdays = weekdays(train$Time)
train$hours = hour(train$Time)

heatmapdata = train %>% group_by(weekdays,hours) %>% summarise(tweets = sum(n()))

summary(heatmapdata)


setwd("S:/Data Science Think Tank/Twitter_visual")
write.csv(heatmapdata,"data/heatmapdata.csv", row.names=FALSE)



library(reshape)
library(knitr)
library(d3heatmap)
heatmapdata$hours = factor(heatmapdata$hours,levels = unique(heatmapdata$hours))
heatmapdata$weekdays = factor(heatmapdata$weekdays ,levels = unique(heatmapdata$weekdays ))

heatmapdata2 = cast(heatmapdata, weekdays ~ hours)
heatmapdata2 
class(heatmapdata2)

d3heatmap(heatmapdata2, dendrogram = "none",color = "Blues")




##################################
###  Social net ###
##################################


# load twitteR to get tweets

tweets <- tweets.df

library(graphTweets)
edges <- graphTweets::getEdges(data = tweets[1:500,], tweets = "text", source = "screenName",str.length = NULL)
edges <- edges[!duplicated(edges),]
edges$source <- as.character(edges$source)
edges$target <- as.character(edges$target)

edges <- edges[(edges$source %in% gsub("((\\.+)[[:space:]]*|[\\.\\-])|[[:punct:]]", "", edges$source, perl=TRUE)) &
                 (edges$target %in% gsub("((\\.+)[[:space:]]*|[\\.\\-])|[[:punct:]]", "", edges$target, perl=TRUE)),]

edges <- edges[(edges$source %in% gsub(ell_def, "", edges$source, perl=TRUE)) &
                 (edges$target %in% gsub(ell_def, "", edges$target, perl=TRUE)),]

edges <- edges[(edges$source!="") & (edges$target!=""),]

freq1 = names(sort(table(edges$source), decreasing=TRUE)[1:5])
freq2 = names(sort( table(edges$target), decreasing=TRUE)[1:5])

edges=edges[edges$target %in% freq2 | edges$source %in% freq1,]
nodes <- getNodes(edges, source = "source", target = "target")


a<-list()
for (i in 1:length(nodes$nodes)){
  print(i)
  a[[i]] = tryCatch({
  expr= getUser(nodes$nodes[i])},  error = function(i) {
  return(NA)
  })
  if(is.na(a[[i]])==TRUE){
    nodes[i,]<-NA
  }
}
nodes$nodes

nodes <- nodes[!is.na(nodes$nodes),]
nodes <- data.frame(nodes)
a<-a[!sapply(a, function(x) all(is.na(x)))]

length(a)
length(nodes$nodes)
#a = sapply(nodes$nodes,getUser)
b = sapply(a, function(x) x$lang)
c = sapply(a, function(x) x$followersCount)
c
nodes$group = b
nodes$size = c/median(c)
nodes$size[nodes$size >20]=20


library(plyr)

edges <- edges[!is.na(edges$source) & !is.na(edges$target),]
nodes = nodes[nodes$nodes %in% unique(c(edges$source,edges$target)),]

edges$source  = as.numeric(mapvalues(edges$source , from = nodes$nodes, to = c(0:(length(nodes$nodes)-1))))
edges$target  = as.numeric(mapvalues(edges$target , from = nodes$nodes, to = c(0:(length(nodes$nodes)-1))))


forceNetwork(Links = edges, Nodes = nodes, Source = "source",
             Target = "target",  NodeID = "nodes",Nodesize="size",
             Group = "group", opacity = 0.8)


write.csv(edges,"data/edges.csv", row.names=FALSE)
write.csv(nodes,"data/nodes.csv", row.names=FALSE)



networkD3::simpleNetwork(edges, Source = "source", Target = "target", width = "100%")

library(networkD3)
data(MisLinks, MisNodes)
forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 0.4)










user <- zillow_mentioners[10]

realtor_christi <- getUser(user)
location(realtor_christi)

christi_followers <- realtor_christi$getFollowers(retryOnRateLimit=180)

christi_followers[1]




userTimeline(christi_followers[15], n=20)

christi_followers_df = rbindlist(lapply(christi_followers,as.data.frame))

head(christi_followers_df)



##################################
###  Social net ###
##################################


data(unemployment)
head( unemployment)

library(highcharter)
hcmap("countries/us/us-all-all", data = unemployment,
      name = "Unemployment", value = "value", joinBy = c("hc-key", "code"),
      borderColor = "transparent") %>%
  hc_colorAxis(dataClasses = color_classes(c(seq(0, 10, by = 2), 50))) %>% 
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0, valueSuffix = "%") 


