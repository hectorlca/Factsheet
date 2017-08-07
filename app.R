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

mensuales <- read.xlsx("data/factsheet.xlsx", 
                       sheetIndex = 1)

homicidios <- select(
  mensuales, timeperiod, numhomicidios) %>%
  na.omit()

homicidios$timeperiod <- ymd(homicidios$timeperiod)
rownames(homicidios) <- homicidios$timeperiod

homicidios <- as.xts(homicidios)

anuales <- read.csv("data/anuales.csv", stringsAsFactors = FALSE)

#### User Interface ####




ui <-
  dashboardPage(
    dashboardHeader(title = "Honduras Factsheet"),
    
    dashboardSidebar(
      width = 200,
      sidebarMenu(
        #style = "position: fixed; overflow: hidden;",
        menuItem(
          "Security",
          tabName = "security",
          icon = icon("user-secret")
        )

        )
      ),
    
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "security",
          h3("Crime and Violence Statistics"),
          fluidRow(
            box(
              width = 6,
              #height = 800,
              dygraphOutput("numhomicidios", height = 400)
            ),
            box (
              width = 6,
              plotlyOutput(
                "tasahom", height = 400)
              )
            )
          )
        )
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