library(shiny)
runApp('S:/Data Science Think Tank/Twitter_visual',host="0.0.0.0",port=5050)



#install.packages('rsconnect')
library(rsconnect)
library(RcppArmadillo)
rsconnect::setAccountInfo(name='zihaozhangap', token='CCC977CDDB18ED55C68DBA2E664C2F6E', secret='/PqGH9VGjl0pqvOh6uOIHmQl3JB+lzGluzAv1Gez')
rsconnect::deployApp("S:/Data Science Think Tank/Twitter_visual")











library(d3heatmap)
library(shiny)

ui <- fluidPage(
  d3heatmapOutput("heatmap"),
 wordcloud2Output("cloudmap")
)

server <- function(input, output, session) {
  
  output$cloudmap <-  renderWordcloud2({
    wordcloud2(data = d)
  })
  
  output$heatmap <- renderD3heatmap({
    d3heatmap::d3heatmap(heatmapdata, dendrogram = "none",color = "Blues")
  })
}

shinyApp(ui, server)

