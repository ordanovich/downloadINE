## Interactive application for programmatic data retrieval from the [INEbase](https://www.ine.es/dyngs/INEbase/listaoperaciones.htm).

:bookmark_tabs: Nomenclature:

- :rocket: [app.R](https://github.com/ordanovich/downloadINE/blob/master/app.R) combining **UI** and **server** parts.
- :fork_and_knife: [global.R](https://github.com/ordanovich/downloadINE/blob/master/global.R) containing helper functions to be used by the [app.R](https://github.com/ordanovich/downloadINE/blob/master/app.R).
- :name_badge: [00_codes_inspireid.RData](https://github.com/ordanovich/downloadINE/raw/master/00_codes_inspireid.RData) with INSPIRE codes and :round_pushpin: [spatial_data_all.RData](https://github.com/ordanovich/downloadINE/raw/master/spatial_data_all.RData) with administrative division of Spain.
- :shipit: [downloadProcedure.R](https://github.com/ordanovich/downloadINE/blob/master/downloadProcedure.R) with fully reproduceable code for bulk data retrieval from the source database using [API JSON INE](https://www.ine.es/dyngs/DataLab/manual.html?cid=45).

The application itself (use `shiny::runApp()` from the cloned repository or opened the [online version](http://193.146.75.235/sample-apps/final_apps/ine_download/) in a new window) pursues the goal to allow users to consult the contents, retrieve and visualize the data in an easy-to-manipulate manner. 
