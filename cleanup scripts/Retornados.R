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

retornados <- as.data.frame(t(retornados))


highchart(retornados)

highchart() %>% 
  hc_chart(type = "bar") %>% 
  hc_title(text = "Adultos Retornados") %>% 
  hc_xAxis(categories = retornados$ano) %>% 
  hc_add_series(data = retornados$ustotal,
                name = "Adultos Retornados") 




hchart(retornados, "column", hcaes(x = ano, y = ustotal))


