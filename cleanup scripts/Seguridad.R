### Proceso de Limpieza Homicidios ###

#Load Libraries#
library(plyr)
library(dplyr)
library(xts)
library(htmlwidgets)
library(dygraphs)
library(lubridate)
library(xlsx)
library(googlesheets)

homicidios <- read.csv("data/limpia/homicidios.csv")

homicidios$mes <- mdy(homicidios$mes)
rownames(homicidios) <- homicidios$mes


homsxts <- as.xts(homicidios)

dygraph(homsxts, main = "Homicides/Day") %>%
  dySeries("homicidios", label = "Homicides") %>%
  dyRangeSelector(height = 30) %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE, width = 350) %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(fillGraph = FALSE)
  dyRangeSelector()
  
