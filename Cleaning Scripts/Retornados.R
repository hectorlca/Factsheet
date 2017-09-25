library(plyr)
library(dplyr)
library(lubridate)
library(chron)
library(tm)
library(ggmap)
library(xts)
library(dygraphs)
library( highcharter)
retornados <- read.csv("data/deportados/retornados.csv")


highchart(retornados)

highchart() %>% 
  hc_chart(type = "pie") %>% 
  hc_title(text = "Adultos Retornados") %>% 
  hc_xAxis(categories = retornados$ano) %>% 
  hc_add_series(data = retornados$usadult,
                name = "Adultos Retornados") %>%
  hc_add_series(data = retornados$usuac,
                name = "UAC Retornados")
