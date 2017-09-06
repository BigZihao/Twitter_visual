

library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Twitter Visual"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Spatial", tabName = "Spatial", icon = icon("th")),
      menuItem("net", tabName = "net", icon = icon("dashboard")),
      menuItem("WordCloud", tabName = "WordCloud", icon = icon("dashboard")),
      menuItem("Heatmap", tabName = "Heatmap", icon = icon("th"))
     
      
      
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "net",
              sidebarLayout(    
              sidebarPanel(
                selectInput("group", "group:", 
                            choices=c("language","gender")),
                hr(),
                helpText("Select groups"),width = 2
              ),
              mainPanel(forceNetworkOutput( "net", width = "130%", height = "400px"))
      )),
      
      tabItem(tabName = "Heatmap",
              mainPanel(d3heatmapOutput("heatmap"))
      ),
    
      tabItem(tabName = "WordCloud",
              mainPanel(wordcloud2Output("cloudmap"))
      ),

      # First tab content
      tabItem(tabName = "Spatial",
              
              div(class="outer",
                  
                  tags$head(
                    # Include our custom CSS
                    includeCSS("styles.css"),
                    includeScript("gomap.js")
                  ),
                  
                  leafletOutput("map", width="100%", height="100%"),
                  
                  # Shiny versions prior to 0.11 should use class="modal" instead.
                  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                width = 330, height = "auto",
                                dateRangeInput('dateRange',
                                               label = 'Date Range',
                                               start = paste(min(train$Time)), end = paste(max(train$Time))),
                                selectInput('cats', 'Sentiment', unique(train$Category), multiple=TRUE, 
                                            selectize=TRUE,selected = "positive")
                                
                  ),
                  
                  tags$div(id="cite",
                           'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
                  )
              )
      )
      
    
    
    )
  )
)


