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

pacman::p_load(RSQLite,magrittr, dplyr, RJSONIO,SOfun, data.table, plyr)

myFun <- function(data) {
                  ListCols <- sapply(data, is.list)
                  cbind(data[!ListCols], t(apply(data[ListCols], 1, unlist)))
}
ms_to_date = function(ms, t0="1970-01-01", timezone) {
                      sec = ms / 1000
                      as.POSIXct(sec, origin=t0, tz=timezone)
}
```

## Available operations

```r


```

