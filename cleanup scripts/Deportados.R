library(plyr)
library(dplyr)
library(lubridate)
library(chron)
library(tm)
library(ggmap)
library(xts)
library(dygraphs)
library (highcharter)


deportados <- read.csv("data/deportados/deportados.csv", stringsAsFactors = FALSE)

#Arregla nombres de columnas

colnames(deportados) <- tolower(colnames(deportados))
col.vector <- gsub(".", "", colnames(deportados), fixed = TRUE)
colnames(deportados) <- col.vector


#Escoger variables

deportados <-
  select(
    deportados,
    anumber,
    controlnumber,
    lastname,
    firstname,
    citizenship,
    dateofbirth,
    birthcity,
    passportnumber,
    gender,
    entrylocation,
    status,
    createddate,
    createdby,
    issueddate,
    issuedby,
    lastupdateddate,
    detentionlocation
  )

# Convertir a formato de fecha

deportados$dob <- dmy(deportados$dateofbirth)


deportados$dob2 <- as.character(deportados$dob)

deportados$dob <- ifelse((year(deportados$dob2)) > 2017,
                         yes = sub("20", "19", deportados$dob2),
                         no = deportados$dob2)

deportados$dob <- as.Date(deportados$dob)



# Calcular edad

deportados$edad <-
  as.numeric(round((today() - deportados$dob) / 365, digits = 1))
deportados$bracket <-
  ifelse(deportados$edad >= 18, yes = "mayor", no = "menor")
rm(col.vector)
deportados <- na.omit(deportados)



# Aqui creo que deberiamos calcular si son menores a partir
#de la fecha que se dio el IssueDate, no today(())

# Resumir:
resumen <-
  group_by(deportados, bracket) %>%
  summarise(numero = n())

# Age Buckets

deportados$bucket <-
  ifelse(deportados$edad <= 17, yes = "0 a 17", no = "none")
deportados$bucket <-
  ifelse(deportados$edad > 17 &
           deportados$edad <= 25,
         yes = "17 a 25",
         no = deportados$bucket)
deportados$bucket <-
  ifelse(deportados$edad > 25 &
           deportados$edad <= 34,
         yes = "26 a 34",
         no = deportados$bucket)
deportados$bucket <-
  ifelse(deportados$edad > 34 &
           deportados$edad <= 44,
         yes = "35 a 44",
         no = deportados$bucket)
deportados$bucket <-
  ifelse(deportados$edad > 44 &
           deportados$edad <= 54,
         yes = "45 a 54",
         no = deportados$bucket)
deportados$bucket <-
  ifelse(deportados$edad > 54 &
           deportados$edad <= 64,
         yes = "55 a 64",
         no = deportados$bucket)
deportados$bucket <-
  ifelse(deportados$edad > 64, yes = "65+", no = deportados$bucket)


# Determinar el consulado donde su salvoconducto fue emitido
deportados$consulado <- "Desconocido"

deportados$consulado <-
  ifelse(deportados$issuedby == "yoliva" |
         deportados$issuedby == "jmenendez",
         yes = "Houston", no = deportados$consulado)

deportados$consulado <-
  ifelse(deportados$issuedby == "abulnes", 
         yes = "McAllen", no = deportados$consulado)

deportados$consulado <-
  ifelse(deportados$issuedby == "mordones" |
           deportados$issuedby == "jmtsaiyu" |
           deportados$issuedby == "rnavarro",
         yes = "Los Angeles", no = deportados$consulado)

deportados$consulado <-
  ifelse(deportados$issuedby == "jvasquez" |
         deportados$issuedby == "mrivera2" |
         deportados$issuedby == "lreyes",   
         yes = "Atlanta", no = deportados$consulado)

deportados$consulado <-
  ifelse(deportados$issuedby == "emiralda", 
         yes = "Seattle", no = deportados$consulado)

deportados$consulado <-
  ifelse(deportados$issuedby == "lgonzalez",
         yes = "New York", no = deportados$consulado)

deportados$consulado <-
  ifelse(deportados$issuedby == "thernandez" |
           deportados$issuedby == "ecalix",
         yes = "New Orleans", no = deportados$consulado)

deportados$consulado <-
  ifelse(deportados$issuedby == "kcerrato" |
           deportados$issuedby == "aagurcia" |
           deportados$issuedby == "cromero",
         yes = "Washington, D.C.", no = deportados$consulado)

deportados$consulado <-
  ifelse(deportados$issuedby == "dfuentes", 
         yes = "Chicago", no = deportados$consulado)


### Limpiar Lugar de Detencion


deportados$detentionlocation <- ifelse(deportados$detentionlocation == "",
                                       yes = "Unknown",
                                       no = deportados$detentionlocation)

deportados$detentionlocation <- tolower(deportados$detentionlocation)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

deportados$detentionlocation <- sapply(deportados$detentionlocation, simpleCap, simplify = TRUE)


### Limpiar lugar de entrada

deportados$entrylocation <- tolower(deportados$entrylocation)
deportados$entrylocation <- sapply(deportados$entrylocation, simpleCap, simplify = TRUE)

unknowns <- contains("unk", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[unknowns, 10] <- "Unknown"

deportados$entrylocation <- gsub(", Az", ", Arizona", deportados$entrylocation)# de Az a Arizona
deportados$entrylocation <- gsub(", Ca", ", California", deportados$entrylocation, ignore.case = TRUE)
deportados$entrylocation <- gsub(", Wa", ", Washington", deportados$entrylocation, ignore.case = TRUE)
deportados$entrylocation <- gsub(", tx", ", Texas", deportados$entrylocation, ignore.case = TRUE)
deportados$entrylocation <- gsub(", nm", ", New Mexico", deportados$entrylocation, ignore.case = TRUE)
deportados$entrylocation <- gsub(", ga", ", Georgia", deportados$entrylocation, ignore.case = TRUE)
deportados$entrylocation <- gsub(", ny", ", New York", deportados$entrylocation, ignore.case = TRUE)
deportados$entrylocation <- gsub(", fl", ", Florida", deportados$entrylocation, ignore.case = TRUE)

deportados$entrylocation <- ifelse(deportados$entrylocation == "",
                                   yes = "Unknown",
                                   no = deportados$entrylocation)

laredos <- starts_with("lar", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[laredos, 10] <- "Laredo, Texas"

hidalgos <- starts_with("hi", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[hidalgos, 10] <- "Hidalgo, Texas"

eagles <- starts_with("ea", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[eagles, 10] <- "Eagle Pass, Texas"

andrade <- starts_with("and", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[andrade, 10] <- "Andrade, California"

brownsville <- contains("nsville", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[brownsville, 10] <- "Brownsville, Texas"

brownsville <- starts_with("br", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[brownsville, 10] <- "Brownsville, Texas"

calexico <- contains("cale", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[calexico, 10] <- "Calexico, California"

carrizo <- starts_with("carr", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[calexico, 10] <- "Carrizo Springs, Texas"

delrio.tx <- contains("Del rio, T", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[delrio.tx, 10] <- "Del Rio, Texas"

elpaso <- contains("el paso", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[elpaso, 10] <- "El Paso, Texas"

falfurrias <- contains("falf", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[falfurrias, 10] <- "Falfurrias, Texas"

lauderdale <- contains("lau", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[lauderdale, 10] <- "Fort Lauderdale, Florida"

houston <- contains("hou", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[houston, 10] <- "Houston, Texas"

indios <- contains("indios", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[indios, 10] <- "Los Indios, Texas"

lukesville <- contains("luke", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[lukesville, 10] <- "Lukesville, Arizona"

nogales <- contains("nog", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[nogales, 10] <- "Nogales, Arizona"

mcallen <- contains("mc", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[mcallen, 10] <- "McAllen, Texas"

miami <- starts_with("miam", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[miami, 10] <- "Miami, Florida"

naco <- starts_with("naco", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[naco, 10] <- "Naco, Arizona"

york <- contains("york", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[york, 10] <- "New York, New York"

progreso <- contains("prog", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[progreso, 10] <- "Progreso, Texas"

rio.grande <- starts_with("rio gran", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[rio.grande, 10] <- "Rio Grande City, Texas"

roma <- starts_with("ro", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[roma, 10] <- "Roma, Texas"

ysidro <- contains("ysi", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[ysidro, 10] <- "San Ysidro, California"

teresa <- contains("teres", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[teresa, 10] <- "Santa Teresa, New Mexico"

borer <- contains("Borer", ignore.case = TRUE, vars = deportados$entrylocation)
deportados[borer, 10] <- "Unknown"

deportados$entrylocation <- gsub("Californialifornia", "California", deportados$entrylocation)

rm(andrade, borer, brownsville, calexico, carrizo, delrio.tx, eagles, elpaso,
   falfurrias, hidalgos, houston, indios, laredos, lauderdale, lukesville, mcallen,
   miami, naco, nogales, progreso, rio.grande, roma, teresa, unknowns, york, ysidro)

#### Ahora, limpiar los nombres de las personas, origenes, y ciudadania ####

deportados$lastname <- tolower(deportados$lastname)
deportados$lastname <- gsub("-", " ", deportados$lastname)
deportados$lastname <- sapply(deportados$lastname, simpleCap)

deportados$firstname <- tolower(deportados$firstname)
deportados$firstname <- gsub("-", " ", deportados$firstname)
deportados$firstname <- sapply(deportados$firstname, simpleCap)

deportados$citizenship <- tolower(deportados$citizenship)
deportados$citizenship <- sapply(deportados$citizenship, simpleCap)

deportados$birthcity <- tolower(deportados$birthcity)
deportados$birthcity <- sapply(deportados$birthcity, simpleCap)

####

deportados$dob2 <- NULL
deportados$dateofbirth <- NULL


### Resumen de Salvoconductos emitidos


write.csv(deportados, "data/limpia/depsclean.csv", row.names = FALSE)

#######################
#### SUMMARIES ########
#######################

deportados <- read.csv("data/limpia/depsclean.csv")


### Take out archived ###

deportados <- filter(deportados, status != "Archived")

### Check for duplicates ###

duplicados <- 
  group_by(deportados, anumber, passportnumber) %>%
  summarise(veces = n())

#########################


depsdaily <- 
  select(deportados, lastname, gender, issueddate)

depsdaily$issuemonth <- month(dmy(depsdaily$issueddate))
depsdaily$issueyear <- year(dmy(depsdaily$issueddate))

depsdaily <- 
  group_by(depsdaily, issuemonth, issueyear) %>%
  summarise( count = n()) %>%
  na.omit

depsdaily$date <- paste0("1/", depsdaily$issuemonth, "/", depsdaily$issueyear)

depsxts <- ungroup(depsdaily)
depsxts <- select(depsdaily, date, count)
depsxts$issuemonth <- NULL


depsxts$date <- dmy(depsxts$date)

rownames(depsxts) <- depsxts$date

#### el Dygraph ###

dygraph(depsxts)
  
  
##################################
###### Cleanup Returnados#########
##################################





ejemplo de cambio hecho por roberto


























