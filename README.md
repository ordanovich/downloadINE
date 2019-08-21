## Interactive application for programmatic data retrieval from [INEbase](https://www.ine.es/dyngs/INEbase/listaoperaciones.htm)

Please refer to [www.ine.es](https://www.ine.es/dyngs/DataLab/en/manual.html?cid=45) to get more information on how the Web Services function and how the [requests for data retrieval](https://www.ine.es/dyngs/DataLab/en/manual.html?cid=48) should be pulled together.

### Nomenclature

- :rocket: [app.R](https://github.com/ordanovich/downloadINE/blob/master/app.R) combining **UI** and **server** parts.
- :earth_americas: [global.R](https://github.com/ordanovich/downloadINE/blob/master/global.R) containing helper functions to be used by the [app.R](https://github.com/ordanovich/downloadINE/blob/master/app.R).
- :name_badge: [00_codes_inspireid.RData](https://github.com/ordanovich/downloadINE/raw/master/00_codes_inspireid.RData) with INSPIRE codes and :round_pushpin: [spatial_data_all.RData](https://github.com/ordanovich/downloadINE/raw/master/spatial_data_all.RData) with administrative division of Spain.
- :shipit: [downloadProcedure.R](https://github.com/ordanovich/downloadINE/blob/master/downloadProcedure.R) with fully reproduceable code for bulk data retrieval from the source database using [API JSON INE](https://www.ine.es/dyngs/DataLab/manual.html?cid=45).

The application itself (use `shiny::runApp()` from the cloned repository or open the <a href="http://193.146.75.235/sample-apps/final_apps/ine_download/"  rel="noopener noreferrer" target="_blank">online version</a> to preview the app) pursues the goal to allow users to consult the contents of the data base, retrieve and visualize the desired datasets in a quick and easy-to-manipulate manner. 

Initialize the process by loading (and installing if needed) the required packages:

```r
source("http://news.mrdwab.com/install_github.R")
devtools::install_github("mrdwab/SOfun")
pacman::p_load(RSQLite, magrittr, dplyr, RJSONIO, SOfun, data.table, plyr)
```

### :one: Available operations

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

### :two: Available tables for the selected operations

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

### :three: Available data series

```r
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
```
### :four: Raw data extraction

```r

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
```

### :five: Data clean-up & re-structuring

```r
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
                  mutate(Fecha = lubridate::as_datetime(
                                  Fecha/1000, 
                                  origin ="1970-01-01", 
                                  tz = "Europe/Madrid"))
    datos_serie.df[[i]] <- x
  }
}

rbindlist(datos_serie.df, fill=T) -> datos_serie.df

strsplit(datos_serie.df$Variable, ". ", fixed = TRUE) -> vars_splitted

rbind.fill(lapply(vars_splitted, function(x)as.data.frame(t(x)))) %>%
                  mutate_if(is.factor, as.character) -> vars_splitted_df

cbind(datos_serie.df, vars_splitted_df) -> datos_serie.df
```

> Note that in the majority of the cases variables would be dot-separated, however some tables might have a different structure.
