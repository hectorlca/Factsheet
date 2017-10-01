library(plyr)
library(dplyr)
library(lubridate)
library(chron)
library(tm)
library(ggmap)
library(xts)
library(dygraphs)
library( highcharter)

aprehensiones <- read.csv("data/deportados/aprehensiones.csv", stringsAsFactors = FALSE)

aprehensiones$mes <- mdy(aprehensiones$mes)
aprehensiones$uac <- NULL
rownames(aprehensiones) <- aprehensiones$mes

appxts <- as.xts(aprehensiones)

dygraph(aprehensiones, main = "Border Apprehensions") %>%
  dySeries("total", label = "Total Apprehended") %>%
  dySeries("menores", label = "Minors") %>%
  dyRangeSelector(height = 30) %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE, width = 350) %>%
  dyOptions(drawGrid = FALSE, fillGraph = FALSE)


  
