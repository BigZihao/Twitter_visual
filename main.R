library(shiny)
runApp('S:/Data Science Think Tank/Twitter_visual',host="0.0.0.0",port=5050)



#install.packages('rsconnect')
library(rsconnect)
library(RcppArmadillo)
rsconnect::setAccountInfo(name='zihaozhangap', token='CCC977CDDB18ED55C68DBA2E664C2F6E', secret='/PqGH9VGjl0pqvOh6uOIHmQl3JB+lzGluzAv1Gez')
rsconnect::deployApp("S:/Data Science Think Tank/Twitter_visual")


