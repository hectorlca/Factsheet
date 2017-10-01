###### Load Libraries ######

library(plyr)
library(plyr)
library(shiny)
library(shinythemes)
library(xts)
library(dygraphs)
library(lubridate)
library(highcharter)
library(plotly)

####################
### Cargar Datos ###
####################

retornados <- read.csv("data/deportados/retornados.csv", stringsAsFactors = FALSE)
etd <- read.csv("data/limpia/resumenetd.csv", stringsAsFactors = FALSE)
aprehensiones <- read.csv("data/deportados/aprehensiones.csv", stringsAsFactors = FALSE)
homicidios <- read.csv("data/limpia/homicidios.csv", stringsAsFactors = FALSE)

################
### Limpieza ###
################

## Aprehensiones ##

aprehensiones$mes <- mdy(aprehensiones$mes)
aprehensiones$uac <- NULL
rownames(aprehensiones) <- aprehensiones$mes
appxts <- as.xts(aprehensiones)

## Retornados ##

retornados$mes <- mdy(retornados$mes)
rownames(retornados) <- retornados$mes
retornadosxts <- as.xts(retornados)

## Homicidios

homicidios$mes <- mdy(homicidios$mes)
rownames(homicidios) <- homicidios$mes
homsxts <- as.xts(homicidios)



######################
### User Interface ###
######################

ui <-
  
  navbarPage(
    "Embajada de Honduras en Washington, DC",
    theme = shinytheme("flatly"),
    tabPanel("Immigration",
             fluidRow(
               column(width = 6, height = 300,
                      dygraphOutput("aprehensiones")
               ),
               column(width = 6, height = 300,
                      dygraphOutput("retornados")
             )
             )
    )
    )
  
###############
### Server ###
##############

server <- function(input, output) {
  
  output$aprehensiones <- renderDygraph({
  
    dygraph(aprehensiones, main = "Border Apprehensions") %>%
      dySeries("total", label = "Total Apprehended") %>%
      dySeries("menores", label = "Minors") %>%
      dyRangeSelector(height = 30) %>%
      dyLegend(show = "always", hideOnMouseOut = FALSE, width = 300) %>%
      dyOptions(drawGrid = FALSE, fillGraph = FALSE)
    
  })
  
  output$retornados <- renderDygraph({
    
    dygraph(retornadosxts, main = "Returned to Honduras") %>%
      dySeries("retornados", label = "Returned") %>%
      dyRangeSelector(height = 30) %>%
      dyLegend(show = "always", hideOnMouseOut = FALSE, width = 180) %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(fillGraph = FALSE) %>%
      dyRangeSelector()
    
    
  })
}
  
  
shinyApp(ui = ui, server = server)  
  





