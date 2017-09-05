
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# server.R
# server.R
library(maps)
library(mapproj)

function(input, output, session) {
  
  output$cloudmap <-  renderPlot({
    
    wordcloud(d$word, d$freq, 
              min.freq = 1,
              max.words=50, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
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
