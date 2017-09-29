library(plyr)
library(dplyr)
library(lubridate)
library(chron)
library(tm)
library(ggmap)
library(xts)
library(dygraphs)
library( highcharter)

### UAC Mensuales

uac <- read.csv("data/deportados/uac.csv")

#uac$mes2 <- as.character(uac$mes)
uac$mes2 <- paste("1/", uac$mes, sep="")
uac$mes <- dmy(uac$mes2)
uac$mes2 <- NULL


##########################################
##### Dygraph UAC Entered Mensuales ######
##########################################
rownames(uac) <- uac$mes
uac <- as.xts(uac)
dygraph(uac)

### UAC by FY ###

uacfy <- read.csv("data/deportados/uacfy.csv", stringsAsFactors = FALSE)

highchart() %>% 
  hc_chart(type = "line") %>% 
  hc_title(text = "Encountered UAC") %>% 
  hc_xAxis(categories = uacfy$FY) %>% 
  hc_add_series(data = as.numeric(uacfy$Honduras),
                name = "Adultos Retornados") 



