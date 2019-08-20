# source("http://news.mrdwab.com/install_github.R")
# devtools::install_github("mrdwab/SOfun")

pacman::p_load(RSQLite,magrittr, dplyr, RJSONIO,SOfun, data.table, plyr)

rm(list = ls())

# busqueda
# https://www.ine.es/consul/inicio.do

## AVAILABLE OPERATIONS ----

operaciones <-  "http://servicios.ine.es/wstempus/js/ES/OPERACIONES_DISPONIBLES"

operaciones <- fromJSON(operaciones)
data.frame(do.call("rbind", operaciones)) -> x
col_flatten(x, names(which(sapply(x, is.list))), drop = TRUE) -> x
operaciones <- as.data.frame(x)

names(operaciones) <- gsub("_1","", names(operaciones))
operaciones$Nombre <- iconv(operaciones$Nombre, from="UTF-8", to="LATIN1")

operaciones$CodIOE <- ifelse(operaciones$Cod_IOE != "", paste0("IOE",operaciones$Cod_IOE), "")

c("IOE30321", # Cifras de Poblacion
  "IOE30245", # Cifras Oficiales de Poblacion de los Municipios Espanoles: Revision del Padron Municipal
  "IOE30453", # Encuesta de Condiciones de Vida (ECV)
  "IOE30414", # Encuesta de Morbilidad Hospitalaria
  "IOE54009", # Encuesta Nacional de Salud (ENSE)
  "IOE30279", # Estadadistica de Adquisiciones de Nacionalidad Espanola de Residentes
  "IOE30417", # Estadadistica de Defunciones segun la Causa de Muerte
  "IOE30259", # Estimaciones de la Poblacion Actual (ePOBa)
  "IOE30264", # Indicadores Demograficos Basicos
  "IOE30306", # MNP Estadastica de Defunciones
  "IOE30302", # MNP Estadastica de Matrimonios
  "IOE30304", # MNP Estadastica de Nacimientos
  "IOE30301", # Movimiento Natural de la Poblacion (Resultados Provisionales)
  "IOE30269", # Proyecciones de Poblacion a Corto Plazo
  "IOE30270"  # Proyecciones de Poblacion a Largo Plazo
) -> tables

operaciones[operaciones$CodIOE %in% tables,c("Nombre", "CodIOE")] -> operaciones 

## AVAILABLE TABLES -----

tt <- list()

for (i in sample(operaciones$CodIOE,1)) {
  
  link <- paste0("http://servicios.ine.es/wstempus/js/ES/TABLAS_OPERACION/",i)
  
  fromJSON(link) -> x
  
  rbindlist(x, fill=TRUE) -> x
  
  if(length(x) > 0) {
    
    x <- as.data.frame(x) %>%
      mutate_if(is.character, function(x) iconv(x, from="UTF-8", to="LATIN1"))
    tt[[i]] <- x
  }
  
  
}

## AVAILABLE SERIES ----

ss <- list()
names(tt) -> nn.tt

for (i in nn.tt) {
  
  for (k in tt[[i]]$Id) {
    
    link <- paste0("http://servicios.ine.es/wstempus/js/ES/SERIES_TABLA/", k)            
    
    
    fromJSON(link) -> x
    
    if(length(x) > 0) {
      
      rbind.fill(lapply(x, function(x)as.data.frame(t(x)))) %>%
        mutate_if(is.character, function(x) iconv(x, from="UTF-8", to="LATIN1")) -> x
      
      ss[[paste0(i,":",k)]] <- x }
    
  }  
  
}

## GETTING DATA FROM THE SERIES ----

datos_serie <- list()

for (i in names(ss)) {
  
  lst <- lapply(ss[[i]] , unlist)
  
  if (length(lst) > 1) {
    
    lst <- as.data.frame(rlist::list.cbind(lst))%>%
      mutate_if(is.factor, as.character) %>%
      mutate_if(is.character, function(x) iconv(x, from="UTF-8", to="LATIN1")) %>%
      dplyr::select(COD, Nombre) %>%
      distinct()
    
    for (t in lst$COD) {
      
      
      link <- paste0("http://servicios.ine.es/wstempus/js/ES/DATOS_SERIE/", t, "?date=19000101:")
      
      fromJSON(link) -> x
      
      if(length(x) > 0) {
        
        name <- lst[lst$COD == t, "Nombre"]
        
        datos_serie[[paste0(i,":",name)]] <- x }
    }
    
  }
  
}


## DATA WRANGLING ----


datos_serie.df <- list()
names(datos_serie) -> loop


for (i in loop) {
  
  x <- as.data.frame(rlist::list.cbind(datos_serie[[i]]$Data))
  if (length(x) > 0) {
    
    x <- lapply(x, unlist)
    table.name <- datos_serie[[i]]$Nombre
    
    x <-  as.data.frame(rlist::list.rbind(x)) %>%
      mutate_if(is.factor, as.character) %>%
      mutate(Variable = table.name) %>%
      mutate_if(is.character, function(x) iconv(x, from="UTF-8", to="LATIN1")) %>%
      mutate(Fecha = lubridate::as_datetime(Fecha/1000, origin ="1970-01-01", tz = "Europe/Madrid")) 

    
    datos_serie.df[[i]] <- x
  }
  
  
}

rbindlist(datos_serie.df, fill=T) -> datos_serie.df

strsplit(datos_serie.df$Variable, ". ", fixed = TRUE) -> vars_splitted

rbind.fill(lapply(vars_splitted, function(x)as.data.frame(t(x)))) -> vars_splitted_df

vars_splitted_df %>% mutate_if(is.factor, as.character) -> vars_splitted_df

cbind(datos_serie.df, vars_splitted_df) -> datos_serie.df


