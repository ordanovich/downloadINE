# source("http://news.mrdwab.com/install_github.R")
# install_github("mrdwab/SOfun")

library(RSQLite) 
library(magrittr)
library(dplyr)
library(RJSONIO)
library(SOfun)
library(data.table)
library(plyr)
library(tidyr)
library(DT)
library(sparkline)
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(htmlwidgets)
library(openxlsx)
library(rmarkdown)
# library(sf)

load("spatial_data_all.RData")
load("00_codes_inspireid.RData")

myModal <- function() {
  div(id = "test",
      modalDialog(downloadButton("download1","Download table as csv"),
                  br(),
                  br(),
                  downloadButton("download2","Download table as xlsx"),
                  
                  easyClose = TRUE, title = "Download Table")
  )
}

myModal2 <- function() {
  div(id = "test2",
      modalDialog(downloadButton("download3","Download table as csv"),
                  br(),
                  br(),
                  downloadButton("download4","Download table as xlsx"),
                  
                  easyClose = TRUE, title = "Download Table")
  )
}

myFun <- function(data) {
  ListCols <- sapply(data, is.list)
  cbind(data[!ListCols], t(apply(data[ListCols], 1, unlist)))
}
ms_to_date = function(ms, t0="1970-01-01", timezone) {
  ## @ms: a numeric vector of milliseconds (big integers of 13 digits)
  ## @t0: a string of the format "yyyy-mm-dd", specifying the date that
  ##      corresponds to 0 millisecond
  ## @timezone: a string specifying a timezone that can be recognized by R
  ## return: a POSIXct vector representing calendar dates and times        
  sec = ms / 1000
  as.POSIXct(sec, origin=t0, tz=timezone)
}


## operaciones -----

operaciones <-  "http://servicios.ine.es/wstempus/js/ES/OPERACIONES_DISPONIBLES"

operaciones <- fromJSON(operaciones)
data.frame(do.call("rbind", operaciones)) -> x
col_flatten(x, names(which(sapply(x, is.list))), drop = TRUE) -> x
operaciones <- as.data.frame(x)
names(operaciones) <- gsub("_1","", names(operaciones))
# operaciones$Nombre <- iconv(operaciones$Nombre, from="UTF-8", to="LATIN1")
operaciones$CodIOE <- ifelse(operaciones$Cod_IOE != "", paste0("IOE",operaciones$Cod_IOE), "")

## population/health related ------

c("IOE30321", # Cifras de PoblaciÃ³n
  "IOE30245", # Cifras Oficiales de PoblaciÃ³n de los Municipios EspaÃ±oles: RevisiÃ³n del PadrÃ³n Municipal
  "IOE30453", # Encuesta de Condiciones de Vida (ECV)
  "IOE30414", # Encuesta de Morbilidad Hospitalaria
  "IOE54009", # Encuesta Nacional de Salud (ENSE)
  "IOE30279", # EstadÃ?stica de Adquisiciones de Nacionalidad EspaÃ±ola de Residentes
  "IOE30417", # EstadÃ?stica de Defunciones segÃºn la Causa de Muerte
  "IOE30259", # Estimaciones de la PoblaciÃ³n Actual (ePOBa)
  "IOE30264", # Indicadores DemogrÃ¡ficos BÃ¡sicos
  "IOE30306", # MNP EstadÃ?stica de Defunciones
  "IOE30302", # MNP EstadÃ?stica de Matrimonios
  "IOE30304", # MNP EstadÃ?stica de Nacimientos
  "IOE30301", # Movimiento Natural de la PoblaciÃ³n (Resultados Provisionales)
  "IOE30269", # Proyecciones de PoblaciÃ³n a Corto Plazo
  "IOE30270"  # Proyecciones de PoblaciÃ³n a Largo Plazo
) -> tables
