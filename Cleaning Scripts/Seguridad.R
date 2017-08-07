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


# Baja la hoja de Excel del Google Drive de la Embajada #
# Cuando se corra este script, tiene que autenticar con: #
# email = embajadadehondurasdc@gmail.com, pass = Departamento19 #

gs_title("factsheet") %>% 
  gs_download(to = "data/factsheet.xlsx")

###################################
#### Importar y Procesar Datos ####
###################################

# Numero de Homicidios #

mensuales <- read.xlsx("data/factsheet.xlsx", 
                sheetIndex = 1)

# Calcular tasas anuales

homicidios <- select(
  mensuales, timeperiod, poblacion, numhomicidios) %>%
  na.omit()

homanuales <- group_by(homicidios, poblacion, year = (year(timeperiod)))
homanuales <- summarise( homanuales, homicidios = sum(numhomicidios))

homanuales$tasa <- round(homanuales$homicidios/(homanuales$poblacion/100000), digits = 1)
homanuales <- arrange(homanuales, year)


#Aqui escribo un archivo solo de homicidios anuales, pero todas las tasas anuales van a ir en el mismo archivo.#
write.csv(homanuales, "data/anuales.csv", row.names = FALSE)


#Tabla de homicidios brutos

homicidios <- select(
  mensuales, timeperiod, numhomicidios) %>%
  na.omit()
  
homicidios$timeperiod <- ymd(homicidios$timeperiod)
rownames(homicidios) <- homicidios$timeperiod

homicidios <- as.xts(homicidios)

# Tasa de Homicidios #





