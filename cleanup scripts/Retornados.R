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

retornados$mes <- mdy(retornados$mes)

rownames(retornados) <- retornados$mes


retornadosxts <- as.xts(retornados)

dygraph(retornadosxts)




