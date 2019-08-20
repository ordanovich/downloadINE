## Interactive application for programmatic data retrieval from the [INEbase](https://www.ine.es/dyngs/INEbase/listaoperaciones.htm).

:bookmark_tabs: Nomenclature:

- :rocket: [app.R](https://github.com/ordanovich/downloadINE/blob/master/app.R) combining **UI** and **server** parts.
- :fork_and_knife: [global.R](https://github.com/ordanovich/downloadINE/blob/master/global.R) containing helper functions to be used by the [app.R](https://github.com/ordanovich/downloadINE/blob/master/app.R).
- :name_badge: [00_codes_inspireid.RData](https://github.com/ordanovich/downloadINE/raw/master/00_codes_inspireid.RData) with INSPIRE codes and :round_pushpin: [spatial_data_all.RData](https://github.com/ordanovich/downloadINE/raw/master/spatial_data_all.RData) with administrative division of Spain.
- :shipit: [downloadProcedure.R](https://github.com/ordanovich/downloadINE/blob/master/downloadProcedure.R) with fully reproduceable code for bulk data retrieval from the source database using [API JSON INE](https://www.ine.es/dyngs/DataLab/manual.html?cid=45).

The application itself (use `shiny::runApp()` from the cloned repository or open the <a href="http://193.146.75.235/sample-apps/final_apps/ine_download/"  rel="noopener noreferrer" target="_blank">online version</a> to preview the app) pursues the goal to allow users to consult the contents of the data base, retrieve and visualize the desired datasets in a quick and easy-to-manipulate manner. 

Initialize the process by loading the required packages and setting up custom functions:

```r
source("http://news.mrdwab.com/install_github.R")
devtools::install_github("mrdwab/SOfun")

pacman::p_load(RSQLite, magrittr, dplyr, RJSONIO, SOfun, data.table, plyr)

myFun <- function(data) {
                  ListCols <- sapply(data, is.list)
                  cbind(data[!ListCols], t(apply(data[ListCols], 1, unlist)))}
                  
ms_to_date = function(ms, t0="1970-01-01", timezone) {
                      sec = ms / 1000
                      as.POSIXct(sec, origin=t0, tz=timezone)}
```

## Available operations

```r
operaciones <-  "http://servicios.ine.es/wstempus/js/ES/OPERACIONES_DISPONIBLES"

operaciones <- fromJSON(operaciones)
data.frame(do.call("rbind", operaciones)) -> x
col_flatten(x, names(which(sapply(x, is.list))), drop = TRUE) -> x
operaciones <- as.data.frame(x)

names(operaciones) <- gsub("_1","", names(operaciones))
operaciones$Nombre <- iconv(operaciones$Nombre, from="UTF-8", to="LATIN1")
operaciones$CodIOE <- ifelse(operaciones$Cod_IOE != "", paste0("IOE",operaciones$Cod_IOE), "")
```
> Here are some of the statistical operations directly related with Population Health aspects and Demographic processes:

```r
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
```

In order to work further only with those datasets you could filter your data frame with the available operations by the corresponding codes:

```r
operaciones[operaciones$CodIOE %in% tables,c("Nombre", "CodIOE")] -> operaciones 
```

## Available tables for selected operations

```r
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
```

> Note that we use the `sample()` function for the demonstration purposes. If you want to loop through the entire selection remove the sampling part.

