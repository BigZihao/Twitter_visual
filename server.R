
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# server.R
# server.R
library(maps)
library(mapproj)
library(d3heatmap)

function(input, output, session) {
  
  output$heatmap <- renderD3heatmap({
    d3heatmap::d3heatmap(heatmapdata, dendrogram = "none",color = "Blues")
  })
  
  

output$net <- renderForceNetwork(forceNetwork(Links = edges, Nodes = nodes, Source = "source",
  Target = "target",  NodeID = "nodes",Nodesize="size",
  Group = input$group, opacity = 0.8))
  
 # output$net <- renderSimpleNetwork(simpleNetwork(edges, Source = "source", Target = "target", width = "100%"))
  
  
  output$cloudmap <-  renderWordcloud2({
    
    wordcloud2(data = d)
  })
  
  

  
 pal <- colorFactor(
    palette = "Set1",
    domain = train$Category
  )
  
  
  filteredData <- reactive({   
    
    train %>% 
      filter_(interp(~ Time >= as.POSIXct(input$dateRange[1], tz = "UTC"), Dates = as.name("Dates"))) %>% 
      filter_(interp(~ Time <= as.POSIXct(input$dateRange[2], tz = "UTC"), Dates = as.name("Dates"))) %>%
      filter_(interp(~ Category == input$cats, Category = as.name("Category")))
      #group_by_(~X,~Y, ~ Category) %>%
      #summarise_(n = interp(~(mean(X)/sum(X))^-1, X=as.name("X")),
      #           popup = interp(~paste0(Dates,": ", Descript," - ", Resolution, "<br>" ,collapse=""), 
       #                         Descript= as.name("Descript"), 
      #                          Resolution = as.name("Resolution")))
  })
  

  
  
 # output$dateRangeText <- renderPrint(({input$dateRange}))
  output$map <- renderLeaflet({
    leaflet(filteredData()) %>% addTiles() %>%
      setView(lng = -93.85, lat = 37.45, zoom = 2)
  })
  
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>% 
      addCircleMarkers(~lon, ~lat, 
                 popup = ~paste(tweet_text), 
                 color=~pal(Category), 
                 radius= ~3*abs(score),
                 stroke=FALSE, fillOpacity=0.4)
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = filteredData())
    pal <- colorFactor(
      palette = "Set1",
      domain = train$Category
    )
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    
    proxy %>% addLegend(position = "bottomright",
                        pal = pal, values = ~Category
    )

  })
}
