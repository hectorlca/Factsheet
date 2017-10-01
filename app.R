library(shiny)
library(shinydashboard)
library(plyr)
library(dplyr)
library(xts)
library(htmlwidgets)
library(dygraphs)
library(lubridate)
library(xlsx)
library(googlesheets)
library(plotly)
library(highcharter)

#######################
#### Read in Data ####
######################


#### Homicidios Anuales y Mensuales ####



#### User Interface ####




ui <-

fluidPage(
  fluidRow(
    
  )
)
  
  
 
    


#### Comienza Servidor ####
    
    
    server <- function(input, output) {
      
      output$numhomicidios <- renderDygraph(
        
        dygraph(homicidios, main = "Total Number of Homicides (Monthly)") %>%
          dySeries(name = "numhomicidios", label = "Homicides") %>%
          dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE) %>%
          dyRangeSelector()
        
      )
      
      output$tasahom <- renderPlotly(
        
        p <- plot_ly(data = anuales,
                     type = "bar",
                     orientation = "v",
                     x = anuales$year,
                     y = anuales$tasa) %>%
        
          layout(
            title = "Homicides Rate per 100k inhabitants.",
            xaxis = list(title = "Year",
                         showgrid = T),
            yaxis = list(title = "Rate")
          )
      )
      
      
      
      }
    
    
    # Run the application 
    shinyApp(ui = ui, server = server)