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
heatmapdata
class(heatmapdata)
heatmapdata[is.na(heatmapdata)] = 0

