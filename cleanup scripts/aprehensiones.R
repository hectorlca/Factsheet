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
rownames(aprehensiones) <- aprehensiones$mes

appxts <- as.xts(aprehensiones)

dygraph(aprehensiones)
