# source("http://news.mrdwab.com/install_github.R")
# install_github("mrdwab/SOfun")

library(RSQLite) 

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
library(highcharter)
library(plotly)
library(sf)
library(magrittr)
library(dplyr)
library(ggplot2)
library(viridis)

source("global.R")


header <- dashboardHeader(disable=T)

sidebar <- dashboardSidebar(disable=T)

body <- dashboardBody(
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  
  tags$head(
    tags$style(HTML(
      ".tabbable ul li:nth-child(1) { float: left; }
      .tabbable ul li:nth-child(2) { float: left; }
      .tabbable > .nav > li > a  {background-color: white;  color:black}"
    ))
  ),
  
  
  fluidRow(
    
    column(width = 12,
           
           timelineBlock(
             
             reversed = TRUE,
             
             timelineStart(color = "gray"),
             
             tags$br(),tags$br(),
             
             # timelineLabel("Step 1", color = "teal"),
             
             timelineItem(
               title = "Consult available statistical operations",
               icon = "laptop-code",
               color = "olive",
               
               boxPlus(title = "Retrieve the list of operations from INEBase",
                       background = "yellow",
                       collapsed = T,
                       collapsible = T,
                       closable = F,
                       width = 12,
                       
                       tags$code("operaciones <- RJSONIO::fromJSON('http://servicios.ine.es/wstempus/js/ES/OPERACIONES_DISPONIBLES')"),
                       tags$br(),
                       tags$code("SOfun::col_flatten(data.frame(do.call('rbind', operaciones)), names(which(sapply(x, is.list))), drop = TRUE) -> x"),
                       tags$br(),
                       tags$code("operaciones <- as.data.frame(x)"),
                       tags$br(),
                       tags$code("names(operaciones) <- gsub('_1','', names(operaciones))"),
                       tags$br(),
                       tags$code("operaciones$CodIOE <- ifelse(operaciones$Cod_IOE != '', paste0('IOE',operaciones$Cod_IOE), '')")
               ),
               
               
               
               
               footer = fluidRow(
                 
                 column(width = 12,
                        
                        fluidRow(
                          
                          column(width = 3,
                                 
                                 radioButtons(inputId = "narrow_list",
                                              label = "Show only population/health related operations",
                                              choices = c("Yes", "No"),
                                              selected = "Yes",
                                              inline = T)
                                 
                          ),
                          
                          
                          column(width = 3,
                                 
                                 htmlOutput("operations_available")
                          ) ,
                          
                          
                          column(width = 3,
                                 
                                 htmlOutput("levels_available")
                          ),
                          
                          column(width = 3,
                                 
                                 htmlOutput("tables_available")
                          ) 
                          
                        )
                 )
               )
             )
             
             
             , timelineItem(
               title = "View and download the list of tables under selected operation",
               icon = "download",
               color = "maroon",
               
               footer = fluidRow(
                 
                 column(width = 12,
                        
                        dataTableOutput("tables_available_DT") %>%
                          withSpinner(color="#DC143C", type = 4)
                 )
               )
             )
             
             
             ,timelineItem(
               title = "Programatically retrieve the data",
               icon = "laptop-code",
               color = "olive",
               
               footer = fluidRow(
                 
                 column(width = 4,
                        
                        boxPlus(title = "Retrieve available series for a particular table",
                                         background = "yellow",
                                         collapsed = T,
                                         collapsible = T,
                                         closable = F,
                                         width = 12,
                                         
                                         fluidRow(
                                           
                                           column(width = 12,
                                                  
                                                  "Select corresponding table code, e.g. id 9663 (Poblacion residente por fecha, sexo y edad -- Cifras de Poblacion)",
                                                  tags$br(),
                                                  tags$code("i <- 9663"),
                                                  tags$br(),
                                                  tags$code("fromJSON(paste0('http://servicios.ine.es/wstempus/js/ES/SERIES_TABLA/', i)) -> x"),
                                                  tags$br(),
                                                  "To make sure the request was completed set a condition to proceed further",
                                                  tags$br(),
                                                  tags$code("if(class(x) == 'list' & length(x) > 0) {"),
                                                  tags$br(),
                                                  "Retrieve and process the data",
                                                  tags$br(),
                                                  tags$code("rbind.fill(lapply(x, function(x) as.data.frame(t(x)))) -> x"),
                                                  tags$br(),
                                                  "Do not forget to wrap up the loop",
                                                  tags$br(),
                                                  tags$code("}")
                                           )
                                         )
                                         
                 )),
                 
                 
                 column(width = 4,    
                        
                        boxPlus(title = "Retrieve available data from the series",
                                background = "yellow",
                                collapsed = T,
                                collapsible = T,
                                closable = F,
                                width = 12,
                                
                                fluidRow(
                                  
                                  column(width = 12,
                                         
                                         "First unlist the object obtained on the previous step",
                                         tags$br(),
                                         tags$code("lst <- lapply(x , unlist)"),
                                         tags$br(),
                                         "Obtain the codes for data series",
                                         tags$br(),
                                         tags$code("lst <- as.data.frame(rlist::list.cbind(lst))%>%"),
                                         tags$br(),
                                         tags$code("mutate_if(is.factor, as.character) %>%"),
                                         tags$br(),
                                         tags$code("dplyr::select(COD, Nombre) %>%"),
                                         tags$br(),
                                         tags$code("distinct()"),
                                         tags$br(),
                                         "Loop through these codes to retrieve all availabla data and store it inside an initially empty list 'datos_serie'",
                                         tags$br(),
                                         tags$code("for (t in lst$COD) {"),
                                         tags$br(),
                                         tags$code("fromJSON(paste0('http://servicios.ine.es/wstempus/js/ES/DATOS_SERIE/', t, '?date=19000101:')) -> x"),
                                         tags$br(),
                                         tags$code("if(length(x) > 0) {name <- lst[lst$COD == t, 'Nombre']; datos_serie[[name]] <- x}"),
                                         tags$br(),
                                         tags$code("}")
                                         
                                  )
                                )
                        )),
                 
                 
                 column(width = 4,
                        
                        boxPlus(title = "Transform the data",
                                background = "yellow",
                                collapsed = T,
                                collapsible = T,
                                closable = F,
                                width = 12,
                                
                                fluidRow(
                                  
                                  column(width = 12,
                                         
                                         "Create an empty list to store your clean data in",
                                         tags$br(),
                                         tags$code("datos_serie.df <- list()"),
                                         tags$br(),
                                         "Iterate through each element of the list filled in the previous step",
                                         tags$br(),
                                         tags$code("for (i in names(datos_serie)) {"),
                                         tags$br(),
                                         tags$code("x <- as.data.frame(rlist::list.cbind(datos_serie[[i]]$Data))"),
                                         tags$br(),
                                         tags$code("if (length(x) > 0) {"),
                                         tags$br(),
                                         tags$code("x <- lapply(x, unlist)"),
                                         tags$br(),
                                         tags$code("table.name <- datos_serie[[i]]$Nombre"),
                                         tags$br(),
                                         tags$code("as.data.frame(rlist::list.rbind(x)) %>%"),
                                         tags$br(),
                                         tags$code("mutate_if(is.factor, as.character) %>%"),
                                         tags$br(),
                                         tags$code("mutate(Variable = table.name) %>%"),
                                         tags$br(),
                                         tags$code("mutate(Fecha = lubridate::as_datetime(Fecha/1000, origin ='1970-01-01', tz = 'Europe/Madrid')) -> x"),
                                         tags$br(),
                                         tags$code("datos_serie.df[[i]] <- x}"),
                                         tags$br(),
                                         tags$code("}"),
                                         tags$br(),
                                         "Separate variables into columns and merge into a uniform data frame",
                                         tags$br(),
                                         tags$code("rbindlist(datos_serie.df, fill=T) -> datos_serie.df"),
                                         tags$br(),
                                         tags$code("strsplit(datos_serie.df$Variable, '. ', fixed = TRUE) -> vars_splitted"),
                                         tags$br(),
                                         tags$code("rbind.fill(lapply(vars_splitted, function(x)as.data.frame(t(x)))) -> vars_splitted_df"),
                                         tags$br(),
                                         tags$code("vars_splitted_df %>% mutate_if(is.factor, as.character) -> vars_splitted_df"),
                                         tags$br(),
                                         tags$code("cbind(datos_serie.df, vars_splitted_df) -> datos_serie.df")
                                  )
                                )
                                )
                        )
                 )
               )
             
             , timelineItem(
               title = "View and download processed data for selected tables",
               icon = "download",
               color = "maroon",
               
               footer = fluidRow(
                 
                 column(width = 12,
                        
                        dataTableOutput("data_DT")%>%
                          withSpinner(color="#DC143C", type = 4)
                 )
               )
             )
             
             
             
             , timelineItem(
               title = "Visualize time-series",
               icon = "chart-area",
               color = "orange",
               
               footer = fluidRow(
                 
                 column(width = 12,
                        
                        highchartOutput("ts") %>%
                          withSpinner(color="#DC143C", type = 4)
                 )
               )
             )
             
             , timelineItem(
               title = "Add geometry and download spatial data",
               icon = "download",
               color = "maroon",
               
               footer = fluidRow(
                 
                 column(width = 12,
                        
                        fluidRow(
                          
                          column(width = 6, htmlOutput("check_var")),
                          
                          column(width = 6, uiOutput("geo_preference"))),
                        
                        fluidRow(
                          
                          column(width = 6, dataTableOutput("check_var_unique_value")),
                          
                          column(width = 6, htmlOutput("geo_vars"))),
                        
                        fluidRow(
                          
                          column(width = 6),
                          
                          column(width = 6
                                 
                                 ,actionButton("act1","Download shapefile")
                                 
                          )
                          
                        )
                 )
               )
             )
             
             # , timelineItem(
             #   title = "Summary map",
             #   icon = "map-marked-alt",
             #   color = "yellow",
             # 
             #   footer = fluidRow(
             # 
             #     column(width = 12,
             # 
             #            fluidRow(
             # 
             #              column(width = 6,
             # 
             #                     htmlOutput("map_grouping_var")
             #              )
             # 
             #            )
             # 
             #            ,fluidRow(
             # 
             #              column(width = 12,
             # 
             #                     plotOutput("map") %>%
             #                       withSpinner(color="#DC143C", type = 4))
             #            )
             #     )
             # 
             #   )
             # )
             
             
             
             ,timelineEnd(color = "danger")
           )
    )
    
  )
)




shinyApp(
  
  ui = dashboardPage(header, sidebar, body),
  
  server = function(input, output, session) { 
    
    options(shiny.usecairo=T)
    
    
    output$operations_available <- renderUI({
      
      if(input$narrow_list == "Yes"){
        
        operaciones %>% dplyr::filter(CodIOE %in% tables) %>% dplyr::select(Nombre) %>% dplyr::distinct() -> tt
        
        selectizeInput(inputId = "operations_available",
                       label = "Choose a statistical operation",
                       choices = tt$Nombre,
                       selected = sample(tt$Nombre,1),
                       multiple = F)
      }
      
      else if(input$narrow_list == "No"){
        
        operaciones %>% dplyr::select(Nombre) %>% dplyr::distinct() -> tt
        
        selectizeInput(inputId = "operations_available",
                       label = "Choose a statistical operation",
                       choices = tt$Nombre,
                       selected = sample(tt$Nombre,1),
                       multiple = F)
      }
      
    })
    
    
    tables_available_data <- reactive({
      
      operaciones %>% dplyr::filter(Nombre == input$operations_available) %>% dplyr::select(CodIOE) -> i; i$CodIOE -> i
      
      link <- paste0("http://servicios.ine.es/wstempus/js/ES/TABLAS_OPERACION/",i)
      
      fromJSON(link) -> x
      
      rbindlist(x, fill=TRUE) -> x
      
      if(length(x) > 0) {
        
        x <- as.data.frame(x) %>%
          mutate_if(is.character, function(x) iconv(x, from="UTF-8", to="LATIN1"))
      }
      
      x
      
    })
    
    
    output$tables_available_DT <- renderDataTable(
      
      
      datatable(tables_available_data(),
                extensions = 'Buttons',
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = list(
                    "copy",
                    list(
                      extend = "collection",
                      text = 'Download entire dataset',
                      action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
                    )
                  )
                )
      ))
    
    
    
    output$levels_available <- renderUI({
      
      
      cc <- unique(tables_available_data()$Codigo)
      
      selectInput(inputId = "levels_available",
                  label = "Select administrative level",
                  choices = cc)
      
      
    })
    
    
    tt <- reactive({
      
      tables_available_data() %>%
        dplyr::filter(Codigo == input$levels_available) %>%
        dplyr::select(Nombre) %>%
        dplyr::distinct() 
      
    })
    
    output$tables_available <- renderUI({
      
      selectizeInput(inputId = "tables_available",
                     label = "Select table/-s to retreieve the data for",
                     choices = tt()$Nombre,
                     selected = sample(tt()$Nombre, 1),
                     multiple = T)
      
      
      
    })
    
    
    data_series_tabla <- reactive({
      
      
      ii <- tables_available_data() %>% dplyr::filter(Nombre %in% input$tables_available) %>% dplyr::select(Id) -> ii
      
      as.character(ii$Id) -> ii
      
      ss <- list()
      
      for (k in 1:length(ii)) {
        
        
        fromJSON(paste0("http://servicios.ine.es/wstempus/js/ES/SERIES_TABLA/", ii[k])) -> x
        
        if(class(x) == "list" & length(x) > 0) {
          
          rbind.fill(lapply(x, function(x) as.data.frame(t(x)))) %>%
            mutate_if(is.character, function(x) iconv(x, from="UTF-8", to="LATIN1")) -> x
          
          ss[[k]] <- x
          
        }
        
      }
      
      names(ss) <- as.character(ii)
      
      datos_serie <- list()
      
      for (s in 1:length(ss)) {
        
        lst <- lapply(ss[[s]] , unlist)
        
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
              
              datos_serie[[paste0(s,":",name)]] <- x }
          }
          
        }
        
      }
      
      
      ## put all data to a uniform list ----
      
      
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
          # dplyr::select(Fecha, Valor, Variable)
          
          datos_serie.df[[i]] <- x
        }
        
        
      }
      
      rbindlist(datos_serie.df, fill=T) -> datos_serie.df
      
      strsplit(datos_serie.df$Variable, ". ", fixed = TRUE) -> vars_splitted
      
      rbind.fill(lapply(vars_splitted, function(x)as.data.frame(t(x)))) -> vars_splitted_df
      
      vars_splitted_df %>% mutate_if(is.factor, as.character) -> vars_splitted_df
      
      cbind(datos_serie.df, vars_splitted_df) -> datos_serie.df
      
      datos_serie.df
      
    })
    
    
    output$data_DT <- renderDataTable(
      
      datatable(data_series_tabla(),
                filter = list(position = 'top', clear = FALSE),
                extensions = 'Buttons',
                options = list(
                  searching = T,
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = list(
                    "copy",
                    list(
                      extend = "collection",
                      text = 'Download entire dataset',
                      action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test2', true, {priority: 'event'});
}")
                    )
                  )
                )
      ))
    
    
    observeEvent(input$test, {
      print("hello")
      showModal(myModal())
    })
    
    
    output$download1 <- downloadHandler(
      filename = function() {
        paste(input$operations_available, ".csv", sep="")
      },
      content = function(file) {
        write.csv(tables_available_data(), file)
      }
    )
    
    output$download2 <- downloadHandler(
      filename = function() {
        paste(input$operations_available, ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx(tables_available_data(), file)
      }
    )
    
    
    
    observeEvent(input$test2, {
      print("hello")
      showModal(myModal2())
    })
    
    
    output$download3 <- downloadHandler(
      filename = function() {
        paste(input$tables_available, ".csv", sep="")
      },
      content = function(file) {
        write.csv(data_series_tabla(), file)
      }
    )
    
    output$download4 <- downloadHandler(
      filename = function() {
        paste(input$tables_available, ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx(data_series_tabla(), file)
      }
    )
    
    
    
    output$ts <- renderHighchart({
      
      highchart(type = "stock") %>%
        
        hc_add_series_df(data_series_tabla() %>% mutate(Fecha = as.Date(Fecha)),
                         "line",
                         x=Fecha,
                         y=Valor,
                         group = Variable,
                         color = Variable) %>%
        # hc_rangeSelector(selected = 2) %>%
        hc_add_theme(hc_theme_flat()) %>%
        hc_legend(enabled = F) %>%
        hc_tooltip(pointFormat = "{point.Variable}:{point.Valor:.0f}")%>%
        hc_title(text = paste0("<b>", input$tables_available, "</b>"),
                 margin = 20, align = "left",
                 style = list(color = "#2b908f", useHTML = TRUE)) %>%
        hc_subtitle(text = "Data sourced from <i>INEBase</i> ",
                    align = "left", style = list(color = "#90ed7d", fontWeight = "bold"))
      
    })
    
    output$check_var <- renderUI({
      
      names(data_series_tabla())[!(names(data_series_tabla()) %in% c("Fecha","FK_TipoDato","FK_Periodo","Anyo","Valor","Secreto","Variable"))] -> vars
      
      radioButtons(inputId = "check_var",
                   label = "Select variable to return unique values for",
                   choices = sort(vars),
                   inline = T)
      
      
    })
    
    output$check_var_unique_value <-  renderDataTable({
      
      datatable(data_series_tabla() %>% dplyr::select(UQ(as.name(input$check_var))) %>% distinct())
      
      
    })
    
    
    output$geo_vars <- renderUI({
      
      names(data_series_tabla())[!(names(data_series_tabla()) %in% c("Fecha","FK_TipoDato","FK_Periodo","Anyo","Valor","Secreto","Variable"))] -> vars
      
      checkboxGroupInput(
        
        inputId = "geo_vars",
        label = "Select variable/-s containing information on geolocation at selected level",
        choices = sort(vars),
        selected = vars[1],
        inline = T)
      
      
    })
    
    output$M <- renderUI({
      
      radioButtons(inputId = "M",
                   label = "Generate spatial data for",
                   choices = c("MUN"))
      
    })
    
    output$MPC <- renderUI({
      
      radioButtons(inputId = "MPC",
                   label = "Generate spatial data for",
                   choices = c("MUN", "PROV", "CCAA"))
      
    })
    
    output$MP <- renderUI({
      
      radioButtons(inputId = "MP",
                   label = "Generate spatial data for",
                   choices = c("MUN", "PROV"))
      
    })
    
    output$MC <- renderUI({
      
      radioButtons(inputId = "MC",
                   label = "Generate spatial data for",
                   choices = c("MUN", "CCAA"))
      
    })
    
    output$P <- renderUI({
      
      radioButtons(inputId = "P",
                   label = "Generate spatial data for",
                   choices = c("PROV"))
      
    })
    
    output$PC <- renderUI({
      
      radioButtons(inputId = "PC",
                   label = "Generate spatial data for",
                   choices = c("PROV", "CCAA"))
      
    })
    
    
    output$C <- renderUI({
      
      radioButtons(inputId = "C",
                   label = "Generate spatial data for",
                   choices = c("CCAA"))
      
    })
    
    
    
    output$geo_preference <- renderUI({
      
      if(grepl("MUN", input$levels_available) == TRUE &
         grepl("PROV", input$levels_available) == FALSE &
         grepl("CCAA", input$levels_available) == FALSE){htmlOutput("M")}
      
      else if(grepl("MUN", input$levels_available) == TRUE &
              grepl("PROV", input$levels_available) == TRUE &
              grepl("CCAA", input$levels_available) == FALSE){htmlOutput("MP")}
      
      else if(grepl("MUN", input$levels_available) == TRUE &
              grepl("PROV", input$levels_available) == TRUE &
              grepl("CCAA", input$levels_available) == TRUE){htmlOutput("MPC")}
      
      else if(grepl("MUN", input$levels_available) == TRUE &
              grepl("PROV", input$levels_available) == FALSE &
              grepl("CCAA", input$levels_available) == TRUE){htmlOutput("MC")}
      
      else if(grepl("MUN", input$levels_available) == FALSE &
              grepl("PROV", input$levels_available) == TRUE &
              grepl("CCAA", input$levels_available) == FALSE){htmlOutput("P")}
      
      else if(grepl("MUN", input$levels_available) == FALSE &
              grepl("PROV", input$levels_available) == TRUE &
              grepl("CCAA", input$levels_available) == TRUE){htmlOutput("PC")}
      
      else if(grepl("MUN", input$levels_available) == FALSE &
              grepl("PROV", input$levels_available) == FALSE &
              grepl("CCAA", input$levels_available) == TRUE){htmlOutput("C")}
      
    })
    
    spatial_data <- reactive({
      
      
      if(grepl("MUN", input$levels_available) == TRUE &
         grepl("PROV", input$levels_available) == FALSE &
         grepl("CCAA", input$levels_available) == FALSE){
        
        
        nn <- input$M
        
        vv_sel <- input$geo_vars
        
        datos_serie.df <- as.data.frame(data_series_tabla())
        
        datos_serie.df$ID <- seq_len(nrow(datos_serie.df)) 
        
        datos_serie.df[, append("ID", vv_sel)] %>% 
          melt(id.vars = "ID") %>%
          mutate_if(is.factor, as.character) %>%
          merge(codes_inspire %>%
                  filter(nivel == nn) %>%
                  dplyr::select(nombre, INSPIREID), by.x = "value", by.y = "nombre", all.x = T) %>%
          dplyr::select(-value) %>%
          dcast(ID ~ variable) -> dd
        
        dd[is.na(dd)] <- ""
        
        tidyr::unite(dd, "INSPIREID", vv_sel, sep = "") -> dd
        
        datos_serie.df %>%
          merge(dd, by = "ID") %>%
          filter(INSPIREID != "") -> datos_serie.dfsp
        
        spain_sf_subset %>%
          merge(datos_serie.dfsp, by = "INSPIREID")
        
      }
      
      else if(grepl("MUN", input$levels_available) == TRUE &
              grepl("PROV", input$levels_available) == TRUE &
              grepl("CCAA", input$levels_available) == FALSE){
        
        nn <- input$MP
        
        vv_sel <- input$geo_vars
        
        datos_serie.df <- as.data.frame(data_series_tabla())
        
        datos_serie.df$ID <- seq_len(nrow(datos_serie.df)) 
        
        datos_serie.df[, append("ID", vv_sel)] %>% 
          melt(id.vars = "ID") %>%
          mutate_if(is.factor, as.character) %>%
          merge(codes_inspire %>%
                  filter(nivel == nn) %>%
                  dplyr::select(nombre, INSPIREID), by.x = "value", by.y = "nombre", all.x = T) %>%
          dplyr::select(-value) %>%
          dcast(ID ~ variable) -> dd
        
        dd[is.na(dd)] <- ""
        
        tidyr::unite(dd, "INSPIREID", vv_sel, sep = "") -> dd
        
        datos_serie.df %>%
          merge(dd, by = "ID") %>%
          filter(INSPIREID != "") -> datos_serie.dfsp
        
        spain_sf_subset %>%
          merge(datos_serie.dfsp, by = "INSPIREID")
        
        
        
      }
      
      else if(grepl("MUN", input$levels_available) == TRUE &
              grepl("PROV", input$levels_available) == TRUE &
              grepl("CCAA", input$levels_available) == TRUE){
        
        nn <- input$MPC
        
        vv_sel <- input$geo_vars
        
        datos_serie.df <- as.data.frame(data_series_tabla())
        
        datos_serie.df$ID <- seq_len(nrow(datos_serie.df)) 
        
        datos_serie.df[, append("ID", vv_sel)] %>% 
          melt(id.vars = "ID") %>%
          mutate_if(is.factor, as.character) %>%
          merge(codes_inspire %>%
                  filter(nivel == nn) %>%
                  dplyr::select(nombre, INSPIREID), by.x = "value", by.y = "nombre", all.x = T) %>%
          dplyr::select(-value) %>%
          dcast(ID ~ variable) -> dd
        
        dd[is.na(dd)] <- ""
        
        tidyr::unite(dd, "INSPIREID", vv_sel, sep = "") -> dd
        
        datos_serie.df %>%
          merge(dd, by = "ID") %>%
          filter(INSPIREID != "") -> datos_serie.dfsp
        
        spain_sf_subset %>%
          merge(datos_serie.dfsp, by = "INSPIREID")
        
      }
      
      else if(grepl("MUN", input$levels_available) == TRUE &
              grepl("PROV", input$levels_available) == FALSE &
              grepl("CCAA", input$levels_available) == TRUE){
        
        nn <- input$MC
        
        vv_sel <- input$geo_vars
        
        datos_serie.df <- as.data.frame(data_series_tabla())
        
        datos_serie.df$ID <- seq_len(nrow(datos_serie.df)) 
        
        datos_serie.df[, append("ID", vv_sel)] %>% 
          melt(id.vars = "ID") %>%
          mutate_if(is.factor, as.character) %>%
          merge(codes_inspire %>%
                  filter(nivel == nn) %>%
                  dplyr::select(nombre, INSPIREID), by.x = "value", by.y = "nombre", all.x = T) %>%
          dplyr::select(-value) %>%
          dcast(ID ~ variable) -> dd
        
        dd[is.na(dd)] <- ""
        
        tidyr::unite(dd, "INSPIREID", vv_sel, sep = "") -> dd
        
        datos_serie.df %>%
          merge(dd, by = "ID") %>%
          filter(INSPIREID != "") -> datos_serie.dfsp
        
        spain_sf_subset %>%
          merge(datos_serie.dfsp, by = "INSPIREID")
        
      }
      
      else if(grepl("MUN", input$levels_available) == FALSE &
              grepl("PROV", input$levels_available) == TRUE &
              grepl("CCAA", input$levels_available) == FALSE){
        
        nn <- input$P
        
        vv_sel <- input$geo_vars
        
        datos_serie.df <- as.data.frame(data_series_tabla())
        
        datos_serie.df$ID <- seq_len(nrow(datos_serie.df)) 
        
        datos_serie.df[, append("ID", vv_sel)] %>% 
          melt(id.vars = "ID") %>%
          mutate_if(is.factor, as.character) %>%
          merge(codes_inspire %>%
                  filter(nivel == nn) %>%
                  dplyr::select(nombre, INSPIREID), by.x = "value", by.y = "nombre", all.x = T) %>%
          dplyr::select(-value) %>%
          dcast(ID ~ variable) -> dd
        
        dd[is.na(dd)] <- ""
        
        tidyr::unite(dd, "INSPIREID", vv_sel, sep = "") -> dd
        
        datos_serie.df %>%
          merge(dd, by = "ID") %>%
          filter(INSPIREID != "") -> datos_serie.dfsp
        
        spain_sf_subset %>%
          merge(datos_serie.dfsp, by = "INSPIREID")
        
      }
      
      else if(grepl("MUN", input$levels_available) == FALSE &
              grepl("PROV", input$levels_available) == TRUE &
              grepl("CCAA", input$levels_available) == TRUE){
        
        nn <- input$PC
        
        vv_sel <- input$geo_vars
        
        datos_serie.df <- as.data.frame(data_series_tabla())
        
        datos_serie.df$ID <- seq_len(nrow(datos_serie.df)) 
        
        datos_serie.df[, append("ID", vv_sel)] %>% 
          melt(id.vars = "ID") %>%
          mutate_if(is.factor, as.character) %>%
          merge(codes_inspire %>%
                  filter(nivel == nn) %>%
                  dplyr::select(nombre, INSPIREID), by.x = "value", by.y = "nombre", all.x = T) %>%
          dplyr::select(-value) %>%
          dcast(ID ~ variable) -> dd
        
        dd[is.na(dd)] <- ""
        
        tidyr::unite(dd, "INSPIREID", vv_sel, sep = "") -> dd
        
        datos_serie.df %>%
          merge(dd, by = "ID") %>%
          filter(INSPIREID != "") -> datos_serie.dfsp
        
        spain_sf_subset %>%
          merge(datos_serie.dfsp, by = "INSPIREID")
        
      }
      
      else if(grepl("MUN", input$levels_available) == FALSE &
              grepl("PROV", input$levels_available) == FALSE &
              grepl("CCAA", input$levels_available) == TRUE){
        
        nn <- input$C
        
        vv_sel <- input$geo_vars
        
        datos_serie.df <- as.data.frame(data_series_tabla())
        
        datos_serie.df$ID <- seq_len(nrow(datos_serie.df)) 
        
        datos_serie.df[, append("ID", vv_sel)] %>% 
          melt(id.vars = "ID") %>%
          mutate_if(is.factor, as.character) %>%
          merge(codes_inspire %>%
                  filter(nivel == nn) %>%
                  dplyr::select(nombre, INSPIREID), by.x = "value", by.y = "nombre", all.x = T) %>%
          dplyr::select(-value) %>%
          dcast(ID ~ variable) -> dd
        
        dd[is.na(dd)] <- ""
        
        tidyr::unite(dd, "INSPIREID", vv_sel, sep = "") -> dd
        
        datos_serie.df %>%
          merge(dd, by = "ID") %>%
          filter(INSPIREID != "") -> datos_serie.dfsp
        
        spain_sf_subset %>%
          merge(datos_serie.dfsp, by = "INSPIREID")
        
      }
      
    })
    
    
    
    output$map_grouping_var <- renderUI({

      selectInput(inputId = "map_grouping_var",
                  label = "Select grouping variable",
                  choices = sort(names(spatial_data())[names(spatial_data()) %in% paste0("V", 1:25)]),
                  selected = "V2"
      )

    })
    
    
    output$map <- renderPlot({

      ggplot(spatial_data() %>%
               mutate(Anyo = lubridate::year(Fecha)) %>%
               dplyr::select(NAMEUNIT, Anyo, UQ(as.name(input$map_grouping_var)), Valor) %>%
               set_colnames(c("Unit", "Year", "GroupingVar", "Value", "geometry")) %>%
               group_by(Unit, Year, GroupingVar) %>%
               summarize(Value = mean(Value, na.rm = T))) +
        geom_sf(aes(fill = Value),lwd = 0) +
        scale_fill_viridis_c(option = "magma",begin = 0.1) +
        facet_wrap(Year ~ GroupingVar, ncol = 10)

    })

    output$download_shp <- downloadHandler(
      filename = "shapefile.zip",
      content = function(file) {
        data = spatial_data() # I assume this is a reactive object
        # create a temp folder for shp files
        temp_shp <- tempdir()
        # write shp files
        writeOGR(data, temp_shp, "trk_buff", "ESRI Shapefile",
                 overwrite_layer = TRUE)
        # zip all the shp files
        zip_file <- file.path(temp_shp, "trk_buff_shp.zip")
        shp_files <- list.files(temp_shp,
                                "trk_buff",
                                full.names = TRUE)
        # the following zip method works for me in linux but substitute with whatever method working in your OS
        zip_command <- paste("zip -j",
                             zip_file,
                             paste(shp_files, collapse = " "))
        system(zip_command)
        # copy the zip file to the file argument
        file.copy(zip_file, file)
        # remove all the files created
        file.remove(zip_file, shp_files)
      }
    )
    
    
  }
  
  
)