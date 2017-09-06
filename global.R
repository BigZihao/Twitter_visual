library(shiny)
library(leaflet)
library(RColorBrewer)
library(lazyeval)
library(dplyr)
library(lubridate)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(reshape)
library(knitr)
library(d3heatmap)
library(graphTweets)
library(wordcloud2)

library(networkD3)


train = read.csv("data/train.csv")
class(train$Time)
train$Time = as.character(train$Time)
train$Time = ymd_hms(train$Time)
summary(train)
class(train$Time)
max(train$Time)
min(train$Time)




d = read.csv("data/clouddata.csv")



heatmapdata = read.csv("data/heatmapdata.csv")
heatmapdata$hours = factor(heatmapdata$hours,levels = unique(heatmapdata$hours))
heatmapdata$weekdays = factor(heatmapdata$weekdays ,levels = unique(heatmapdata$weekdays ))
heatmapdata = cast(heatmapdata, weekdays ~ hours)
heatmapdata[is.na(heatmapdata)] <- round(runif(sum(is.na(heatmapdata)),0.5* min(heatmapdata[1,-1]), 0.5*max(heatmapdata[1,-1])))
class(heatmapdata)


edges = read.csv("data/edges.csv", stringsAsFactors = FALSE)
nodes = read.csv("data/nodes.csv", stringsAsFactors = FALSE)
nodes$language <- nodes$group
nodes$gender <- "male"
nodes$gender[sample(1:dim(nodes)[1],round(0.55*dim(nodes)[1]))]<-"female"
nodes$gender[sample(1:dim(nodes)[1],round(0.1*dim(nodes)[1]))]<-"unknow"
nodes$gender[nodes$nodes=="zillow"]<-"unknow"

