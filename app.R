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

retornados <-
  read.csv("data/deportados/retornados.csv", stringsAsFactors = FALSE)
etd <-
  read.csv("graficos/immigration/etdsmonthly.csv", stringsAsFactors = FALSE)
aprehensiones <-
  read.csv("data/deportados/aprehensiones.csv", stringsAsFactors = FALSE)
homicidios <-
  read.csv("data/limpia/homicidios.csv", stringsAsFactors = FALSE)
gdp <- 
  read.csv("graficos/economy/gdp.csv", stringsAsFactors = FALSE)
gdprem <- 
  read.csv("graficos/economy/gdprem.csv", stringsAsFactors = FALSE)




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
    theme = shinytheme("journal"),
    
    #### Immigration Tab ####
    
    tabPanel("Immigration",
             
             fluidRow(
               column(width = 2, height = 300),
               
               
               column(
                 width = 8,
                 height = 600,
                 
                 tabsetPanel(
                   tabPanel("Returnees",
                            highchartOutput("retornados", height = "500px")),
                   
                   tabPanel(
                     "Border Apprehensions",
                     highchartOutput("aprehensiones", height = "500px")
                   ),
                   
                   tabPanel(
                     "Electronic Travel Documents",
                     highchartOutput("etd", height = "500px")
                   )
                 )
               ),
               
               column(width = 2, height = 300)
               
               
               
             )),
    
    #### Economy Tab ####
    
    tabPanel("Economy",
             
             fluidRow(
               column(width = 2, height = 300),
               
               
               column(
                 width = 8,
                 height = 600,
                 
                 tabsetPanel(
                   tabPanel("GDP Growth",
                            highchartOutput("gdpchart", height = "500px")),
                   
                   tabPanel(
                     "Remittances",
                     highchartOutput("remittanceschart", height = "500px")
                   ),
                   
                   tabPanel(
                     "Trade Balance",
                     highchartOutput("tradechart", height = "500px")
                   ),
                   
                   tabPanel(
                     "Foreign Investment",
                     highchartOutput("fdichart", height = "500px")
                   )
                   
                 )
               ),
               
               column(width = 2, height = 300)
               
             ))
    
    
  )



###############
### Server ###
##############

server <- function(input, output, session) {
  
  
  
  #### Immigration Graphs ####
  
  output$aprehensiones <- renderHighchart({
    highchart() %>% hc_add_theme(hc_theme_smpl()) %>%
      
      hc_xAxis(categories = aprehensiones$month) %>%
      hc_add_series(data = aprehensiones$total,
                    name = "Total Apprehended") %>%
      hc_add_series(data = aprehensiones$menores,
                    name = "Minors Apprehended") %>%
      hc_chart(type = "line") %>%
      hc_title(
        text = "Monthly Apprehensions at the Southwest Border",
        style = list(
          color = "#2d323a",
          fontSize = "27px",
          fontFamily = "Franklin Gothic Medium Cond",
          style = "bold"
        )
      ) %>%
      hc_subtitle(
        text = "Source: CBP Biweekly report.",
        style = list(
          color = "#525c6b",
          fontSize = "17px",
          fontFamily = "Times New Roman",
          style = "bold"
        )
      ) %>%
      hc_tooltip(
        animation = TRUE,
        followPointer = FALSE,
        headerFormat = '<span style="font-size: 14px">{point.key}</span><br/>',
        shared = TRUE
      )
    
  })
  
  output$retornados <- renderHighchart({
    highchart() %>% hc_add_theme(hc_theme_smpl()) %>%
      
      hc_xAxis(categories = retornados$month) %>%
      hc_add_series(
        data = retornados$totalretornados,
        name = "Total Returnees",
        fillOpacity = 0.3
      ) %>%
      hc_add_series(
        data = retornados$retornadosmex,
        name = "Returnees from Mexico and CA",
        fillOpacity = 0.5
      ) %>%
      hc_add_series(
        data = retornados$retornadosus,
        name = "Returnees from US",
        fillOpacity = 0.7
      ) %>%
      
      hc_chart(type = "area") %>%
      
      hc_title(
        text = "Honduran Returnees",
        style = list(
          color = "#2d323a",
          fontSize = "27px",
          fontFamily = "Franklin Gothic Medium Cond",
          style = "bold"
        )
      ) %>%
      hc_subtitle(
        text = "Source: Honduras Ministry of Foreign Affairs.",
        style = list(
          color = "#525c6b",
          fontSize = "17px",
          fontFamily = "Times New Roman",
          style = "bold"
        )
      ) %>%
      hc_tooltip(
        animation = TRUE,
        followPointer = FALSE,
        headerFormat = '<span style="font-size: 14px">{point.key}</span><br/>',
        shared = TRUE
      )
    
    
  })
  
  output$etd <- renderHighchart({
    highchart() %>% hc_add_theme(hc_theme_smpl()) %>%
      
      hc_xAxis(categories = etd$month) %>%
      hc_add_series(data = etd$count,
                    name = "Total ETD's Issed") %>%
      
      hc_chart(type = "line") %>%
      
      hc_title(
        text = "ETD's Issued By Month",
        style = list(
          color = "#2d323a",
          fontSize = "27px",
          fontFamily = "Franklin Gothic Medium Cond",
          style = "bold"
        )
      ) %>%
      hc_subtitle(
        text = "ICE ETD Platform",
        style = list(
          color = "#525c6b",
          fontSize = "17px",
          fontFamily = "Times New Roman",
          style = "bold"
        )
      ) %>%
      hc_tooltip(
        animation = TRUE,
        followPointer = FALSE,
        headerFormat = '<span style="font-size: 14px">{point.key}</span><br/>',
        shared = TRUE
      )
    
  })
  
  #### Economy Graphs ####
  
  output$gdpchart <- renderHighchart({
    highchart() %>% hc_add_theme(hc_theme_smpl()) %>%
      
      hc_xAxis(categories = gdp$year) %>%
      hc_add_series(data = gdp$gdpgrowth,
                    name = "GDP Growth") %>%
      
      hc_chart(type = "line") %>%
      
      hc_title(
        text = "Real GDP Growth",
        style = list(
          color = "#2d323a",
          fontSize = "27px",
          fontFamily = "Franklin Gothic Medium Cond",
          style = "bold"
        )
      ) %>%
      hc_subtitle(
        text = "Source: World Bank/IMF",
        style = list(
          color = "#525c6b",
          fontSize = "17px",
          fontFamily = "Times New Roman",
          style = "bold"
        )
      ) %>%
      hc_tooltip(
        animation = TRUE,
        followPointer = FALSE,
        headerFormat = '<span style="font-size: 14px">{point.key}</span><br/>',
        valueSuffix = "%",
        shared = TRUE
      )
  })
  
  output$remittanceschart <- renderHighchart({
    
    highchart() %>% hc_add_theme(hc_theme_smpl())%>%
      
      hc_xAxis(categories = gdprem$year) %>% 
      hc_add_series(data = gdprem$remgdp * 100,
                    name = "Remmitances as % of GDP") %>%
      
      hc_chart(type = "line") %>% 
      
      hc_title(text = "Remmitances as a percent of GDP",
               style = list(color = "#2d323a", fontSize = "27px", 
                            fontFamily = "Franklin Gothic Medium Cond", style = "bold")) %>%
      hc_subtitle(text = "Source: Honduran Central Bank", 
                  style = list(color = "#525c6b", fontSize = "17px", 
                               fontFamily = "Times New Roman", style = "bold")) %>%
      hc_tooltip(animation = TRUE,
                 followPointer = FALSE,
                 headerFormat = '<span style="font-size: 14px">{point.key}</span><br/>', 
                 shared = TRUE)
    
  })
  
  output$tradechart <- renderHighchart({
    
    highchart() %>% hc_add_theme(hc_theme_smpl())%>%
      
      hc_xAxis(categories = gdprem$year) %>% 
      hc_add_series(data = round(gdprem$imports/1000, 1),
                    name = "Total Imports") %>%
      hc_add_series(data = round(gdprem$exports/1000, 1),
                    name = "Total Exports") %>%
      hc_add_series(data = round((gdprem$imports - gdprem$exports)/1000, 1),
                    name = "Trade Deficit", color = "lightgray") %>%
      hc_chart(type = "line") %>% 
      
      hc_title(text = "Honduras Trade Balance (in billions of USD)",
               style = list(color = "#2d323a", fontSize = "27px", 
                            fontFamily = "Franklin Gothic Medium Cond", style = "bold")) %>%
      hc_subtitle(text = "Source: Honduras Central Bank", 
                  style = list(color = "#525c6b", fontSize = "17px", 
                               fontFamily = "Times New Roman", style = "bold")) %>%
      hc_tooltip(animation = TRUE,
                 followPointer = FALSE,
                 headerFormat = '<span style="font-size: 14px">{point.key}</span><br/>', 
                 shared = TRUE, split = TRUE, 
                 valuePrefix = "$", valueSuffix = " billion")
  })
  
  output$fdichart <- renderHighchart({
    
    gdprem$fdigdp <- round(gdprem$fdi/gdprem$gdp*100, 1)
    
    highchart() %>% hc_add_theme(hc_theme_smpl())%>%
      
      hc_xAxis(categories = gdprem$year) %>% 
      hc_add_series(data = gdprem$fdigdp,
                    name = "FDI as % of GDP") %>%
      
      hc_chart(type = "line") %>% 
      
      hc_title(text = "Foreign Direct Investment as a percent of GDP",
               style = list(color = "#2d323a", fontSize = "27px", 
                            fontFamily = "Franklin Gothic Medium Cond", style = "bold")) %>%
      hc_subtitle(text = "Source: Honduran Central Bank", 
                  style = list(color = "#525c6b", fontSize = "17px", 
                               fontFamily = "Times New Roman", style = "bold")) %>%
      hc_tooltip(animation = TRUE,
                 followPointer = FALSE,
                 headerFormat = '<span style="font-size: 14px">{point.key}</span><br/>', 
                 shared = TRUE)
  })
  
  
  
  
  
  
  
  
  
  
}


shinyApp(ui = ui, server = server)
