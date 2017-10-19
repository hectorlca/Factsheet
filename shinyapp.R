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
etd <- read.csv("graficos/immigration/etdsmonthly.csv", stringsAsFactors = FALSE)
aprehensiones <- read.csv("data/deportados/aprehensiones.csv", stringsAsFactors = FALSE)
homicidios <- read.csv("data/limpia/homicidios.csv", stringsAsFactors = FALSE)

################
### Limpieza ###
################

## Aprehensiones ##

#aprehensiones$mes <- mdy(aprehensiones$mes)
#aprehensiones$uac <- NULL
#rownames(aprehensiones) <- aprehensiones$mes
#appxts <- as.xts(aprehensiones)

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
               column(width = 2, height = 300)
                      #highchartOutput("aprehensiones")
               ,
               column(width = 8, height = 300,
                      highchartOutput("retornados")),
                      
               column(width = 4, height = 300)
                      #highchartOutput("etd"))        
             
             )
    )
    )
  
###############
### Server ###
##############

server <- function(input, output) {
  
  output$aprehensiones <- renderHighchart({
  
    highchart() %>% hc_add_theme(hc_theme_smpl())%>%
      
      hc_xAxis(categories = aprehensiones$month) %>% 
      hc_add_series(data = aprehensiones$total,
                    name = "Total Apprehended") %>%
      hc_add_series(data = aprehensiones$menores,
                    name = "Minors Apprehended") %>%
      hc_chart(type = "line") %>% 
      hc_title(text = "Monthly Apprehensions at the Southwest Border",
               style = list(color = "#2d323a", fontSize = "27px", 
                            fontFamily = "Franklin Gothic Medium Cond", style = "bold")) %>%
      hc_subtitle(text = "Source: CBP Biweekly report.", 
                  style = list(color = "#525c6b", fontSize = "17px", 
                               fontFamily = "Times New Roman", style = "bold")) %>%
      hc_tooltip(animation = TRUE,
                 followPointer = FALSE,
                 headerFormat = '<span style="font-size: 14px">{point.key}</span><br/>', 
                 shared = TRUE)
    
  })
  
  output$retornados <- renderHighchart({
    
    highchart() %>% hc_add_theme(hc_theme_smpl())%>%
      
      hc_xAxis(categories = retornados$month) %>% 
      hc_add_series(data = retornados$totalretornados,
                    name = "Total Returnees", fillOpacity = 0.3) %>%
      hc_add_series(data = retornados$retornadosmex,
                    name = "Returnees from Mexico and CA", fillOpacity = 0.5) %>%
      hc_add_series(data = retornados$retornadosus,
                    name = "Returnees from US", fillOpacity = 0.7) %>%
      
      hc_chart(type = "area") %>% 
      
      hc_title(text = "Honduran Returnees",
               style = list(color = "#2d323a", fontSize = "27px", 
                            fontFamily = "Franklin Gothic Medium Cond", style = "bold")) %>%
      hc_subtitle(text = "Source: Honduras Ministry of Foreign Affairs.", 
                  style = list(color = "#525c6b", fontSize = "17px", 
                               fontFamily = "Times New Roman", style = "bold")) %>%
      hc_tooltip(animation = TRUE,
                 followPointer = FALSE,
                 headerFormat = '<span style="font-size: 14px">{point.key}</span><br/>', 
                 shared = TRUE)
    
    
  })
  
  output$etd<- renderHighchart({
    
    highchart() %>% hc_add_theme(hc_theme_smpl())%>%
      
      hc_xAxis(categories = etd$month) %>% 
      hc_add_series(data = etd$count,
                    name = "Total ETD's Issed") %>%
      
      hc_chart(type = "line") %>% 
      
      hc_title(text = "ETD's Issued By Month",
               style = list(color = "#2d323a", fontSize = "27px", 
                            fontFamily = "Franklin Gothic Medium Cond", style = "bold")) %>%
      hc_subtitle(text = "ICE ETD Platform", 
                  style = list(color = "#525c6b", fontSize = "17px", 
                               fontFamily = "Times New Roman", style = "bold")) %>%
      hc_tooltip(animation = TRUE,
                 followPointer = FALSE,
                 headerFormat = '<span style="font-size: 14px">{point.key}</span><br/>', 
                 shared = TRUE)
    
  })
}
  
  
shinyApp(ui = ui, server = server)  
  





