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



train = read.csv("data/train.csv")
class(train$Time)
train$Time = as.character(train$Time)
train$Time = ymd_hms(train$Time)
summary(train)
class(train$Time)
max(train$Time)
min(train$Time)




d = read.csv("data/clouddata.csv")
