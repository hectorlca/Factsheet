library(plyr)
library(dplyr)
library(lubridate)
library(chron)
library(tm)
library(ggmap)
library(xts)
library(dygraphs)
library( highcharter)

etd <- read.csv("data/limpia/resumenetd.csv", stringsAsFactors = FALSE)

etd$date <- mdy(etd$date)
rownames(etd) <- etd$date

dygraph(etd)

