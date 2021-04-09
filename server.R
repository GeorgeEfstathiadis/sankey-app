source('init.R')

library(shiny, warn.conflicts = FALSE)
library(shinydashboard, warn.conflicts = FALSE)
library(shinyWidgets, warn.conflicts = FALSE)
library(shinyBS, warn.conflicts = FALSE)

library(dplyr, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)
library(readxl, warn.conflicts = FALSE)
library(haven, warn.conflicts = FALSE)

library(htmltools, warn.conflicts = FALSE)
library(htmlwidgets, warn.conflicts = FALSE)

library(networkD3, warn.conflicts = FALSE)
library(highcharter, warn.conflicts = FALSE)
library(RColorBrewer, warn.conflicts = FALSE)
library(DT, warn.conflicts = FALSE)


server <- function(input, output, session){
  options(shiny.maxRequestSize=500*1024^2)
  options(dplyr.summarise.inform = FALSE)
  # load R template for download
  myDir <- tempdir()
  file.copy('www/temp_wide.R', myDir)
  file.copy('www/temp_long.R', myDir)

  # Need server to not prompt H15 error in heroku
  output$keep_alive <- renderText({
    req(input$alive_count)
    input$alive_count
  })


  ### Input Page and Filters Actions ###
  

  # load uploaded data
  ## check file format and load depending of type
  data_uploaded <- reactive({
    validate(
      need(input$data, 'No data loaded')
    )
    
    file <- input$data
    ext <- tools::file_ext(file$datapath)
    if (ext == 'csv'){
      res <- read.csv(file$datapath, header = TRUE)
    }else if (ext == 'sas7bdat') {
      res <- data.frame(read_sas(file$datapath))
    }else if (ext == 'xlsx'){
      res <- read_excel(file$datapath)
    }
    res
  })
  
  # show data table
  output$contents <- DT::renderDataTable({
    validate(
      need(input$data, 'No data loaded')
    )
    
    DT::datatable(data_uploaded(), 
                  extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
                  options = list(
                    # dom = 't',
                    # deferRender = TRUE,
                    searching = TRUE,
                    autoWidth = TRUE,
                    # scrollCollapse = TRUE,
                    rownames = FALSE,
                    scroller = TRUE,
                    scrollX = TRUE,
                    scrollY = "500px",
                    class = 'cell-border stripe'
                  )
    )
    
  })
  
  # when loading data
  ## update options in select main cols 
  ## update nodes colors
  ## if cols with desired names in data select them automatically
  ## remove file input widget
  ## add refresh button on top of data table
  observeEvent(input$data,
               {
                 res <- data_uploaded()
                 cols <- res %>% colnames() 
                 
                 
                 updatePickerInput(session,
                                   "usubjid",
                                   choices = cols
                                   )
                 updatePickerInput(session,
                                   "node_s",
                                   choices = cols
                                   )
                 updatePickerInput(session,
                                   "node_e",
                                   choices = cols
                                   )
                 
                 updatePickerInput(session,
                                   "pathname",
                                   choices = c('',cols)
                                  )
                 updatePickerInput(session,
                                   "filters",
                                   choices = cols)

                 updatePickerInput(session,
                                   "pathno",
                                   choices = cols)
                 
                 #insert color options for nodes 
                 colors <- c("#ffe4c4", "#00FFBF", "#FF7F50", "#4682b4", "#ff4500", "#583687",
                                "#acb839", "#6f71d9","#6fb24b","#c45cad","#46ca79","#c9407d","#46b57c","#d43f61","#36dee6","#c2562e","#43c29e","#d85750","#71b46c",
                                "#bc80d5","#406e21","#5e87d3","#d1972c","#7f265b","#99b257","#8c233e","#b7a850","#d8739f","#836723","#d16578","#c8873a","#88212a","#da905e",
                                "#8d3820","#d56467")
                 
                 contents_colors <- paste0("<div style='background: ", colors, "; color: white; padding-left: 5px;'>",colors ,"</div>")
                 style_colors <- final_c <- paste0('background: ', colors)
                 
                
                 updatePickerInput(session, 
                                   'color_1',
                                   choices = colors,
                                   choicesOpt = list(
                                     content = contents_colors,
                                     style = style_colors
                                   ),
                                   selected = colors[1])
                 updatePickerInput(session, 
                                   'color_2',
                                   choices = colors,
                                   choicesOpt = list(
                                     content = contents_colors,
                                     style = style_colors
                                   ),
                                   selected = colors[4])
                 updatePickerInput(session, 
                                   'color_3',
                                   choices = colors,
                                   choicesOpt = list(
                                     content = contents_colors,
                                     style = style_colors
                                   ),
                                   selected = colors[5])
                 updatePickerInput(session, 
                                   'color_4',
                                   choices = colors,
                                   choicesOpt = list(
                                     content = contents_colors,
                                     style = style_colors
                                   ),
                                   selected = colors[7])
                 
                 if ('USUBJID' %in% cols){
                   updatePickerInput(session,
                                     'usubjid',
                                     selected = 'USUBJID')
                 }
                 if ('NODE_S' %in% cols){
                   updatePickerInput(session,
                                     'node_s',
                                     selected = 'NODE_S')
                 }
                 if ('NODE_E' %in% cols){
                   updatePickerInput(session,
                                     'node_e',
                                     selected = 'NODE_E')
                 }
                 if ('PATHNAME' %in% cols){
                   updatePickerInput(session,
                                     'pathname',
                                     selected = 'PATHNAME')
                 }

                 if ('PATHNO' %in% cols){
                   updatePickerInput(session,
                                     'pathno',
                                     selected = 'PATHNO')
                 }


                 
                 removeUI(
                   selector = 'div:has(> #data_progress)'
                          )
                 
                 insertUI(
                   selector = '#addRefresh',
                   where = 'afterEnd',
                   ui = actionBttn(
                     inputId = "refresh",
                     label = "Refresh Page",
                     style = "pill", 
                     color = "primary"
                   )
                   
                 )
                
               })
  
 
  # refresh session button
  observeEvent(input$refresh, {
    if (input$refresh != 0){
      session$reload()
    }
  })

  ## previous choices for filters
  v <- reactiveValues(choices = c())
  
  # filters update
  ## adds and removes filters from filters tab depending of the filters selector list
  ## if numeric add sliders, if character add multi select
  observeEvent(input$filters,{
    choices <- v$choices
    filters <- input$filters
    data <- data_uploaded()
    data2 <- data[1:5,]
    
    ## action when adding a filter
    if (length(choices) == (length(filters) - 1)){
      c <- filters[!(filters %in% choices)]
      c_name <- paste(c, 'filter', sep = '_')
      type <- class(data2[,c])

      if (type == 'integer'|type == 'numeric'){
        minimum <- min(data[,c], na.rm = TRUE)
        maximum <- max(data[,c], na.rm = TRUE)
        insertUI(
          selector = '#addHere',
          where = 'afterEnd',
          ui = sliderInput(c_name,
                               label = c, 
                               min = minimum, 
                               max = maximum,
                               value = c(minimum, maximum),
                               ticks = FALSE
                               )
        )
        
      } else if(type == 'logical' | type == 'factor' | type == 'character'){
        insertUI(
          selector = '#addHere',
          where = 'afterEnd',
          ui = pickerInput(c_name,
                           label = c,
                           choices = levels(unique(factor(data[,c]))),
                           multiple = TRUE,
                           options = list(`selected-text-format` = "count > 2", `live-search` = TRUE)
                           
          )
        )
      }
      ## action when removing filter
    } else if (length(choices) == (length(filters) + 1)) {
      c <- choices[!(choices %in% filters)]
      c_name <- paste(c, 'filter', sep = '_')
      type <- class(data2[,c])

      if (type == 'integer'|type == 'numeric'){
        removeUI(
          selector = paste0('div:has(> #', c_name, ')')
        )
        
      } else if(type == 'logical' | type == 'factor' | type == 'character'){
        removeUI(
          selector = paste0('div:has(>div:has(> #', c_name, '))')
        )
      }
      ## bug when adding filter to empty filters list for a second or later time
    } else if (length(choices) == length(filters)) {
      c <- choices[1]
      f <- filters[1]
      c_name <- paste(c, 'filter', sep = '_')
      type <- class(data2[,c])
      f_name <- paste(f, 'filter', sep = '_')
      type2 <- class(data2[,f])

      ## remove previous choice
      if (type == 'integer'|type == 'numeric'){
        removeUI(
          selector = paste0('div:has(> #', c_name, ')')
        )
        
      } else if(type == 'logical' | type == 'factor' | type == 'character'){
        removeUI(
          selector = paste0('div:has(>div:has(> #', c_name, '))')
        )
      }

      ## add new choice
      if (type2 == 'integer'|type2 == 'numeric'){
        minimum <- min(data[,f], na.rm = TRUE)
        maximum <- max(data[,f], na.rm = TRUE)
        insertUI(
          selector = '#addHere',
          where = 'afterEnd',
          ui = sliderInput(f_name,
                               label = f, 
                               min = minimum, 
                               max = maximum,
                               value = c(minimum, maximum),
                               ticks = FALSE
                               )
        )
        
      } else if(type2 == 'logical' | type2 == 'factor' | type2 == 'character'){
        insertUI(
          selector = '#addHere',
          where = 'afterEnd',
          ui = pickerInput(f_name,
                           label = f,
                           choices = levels(unique(factor(data[,f]))),
                           multiple = TRUE,
                           options = list(`selected-text-format` = "count > 2", `live-search` = TRUE)
                           
          )
        )
      }
    } 
    ## adding new filters to previous ones for next iteration
    v$choices <- filters 
  })
  
 
  
  # insert path range filter
  observeEvent(c(input$data, input$pathno),{
    validate(
      need(input$data, ''),
      need(input$pathno != '', '')
    )
    
    data <- data_uploaded() 
    pathno <- input$pathno
    cl <- data %>%
      pull(pathno) %>%
      class()
    if(cl %in% c('integer', "numeric")){
      
      paths <- data %>% 
        pull(pathno) %>% 
        unique() %>%
        sort()
      numlist <- prettyNum(paths, big.mark = ",")
      
      removeUI(
        selector = 'div:has(> #path_range)'
      )
      
      insertUI(
        selector = '#path_range_here',
        where = 'afterEnd',
        ui = sliderTextInput('path_range',
                             label = 'Path Range', 
                             choices = numlist,
                             selected = c(numlist[1], numlist[length(numlist)]),
                             grid = FALSE, dragRange = FALSE)
      )
      
      
    } else{
      showNotification('Pathno not an integer', type='error')
    }
    
    
    
  })
  
  ## switch tab when pressing update
  observeEvent(input$update,{
    newtab <- switch(input$tabs,
                     "inputs" = "dashboard",
                     "help" = "dashboard"
    )
    updateTabItems(session, "tabs", newtab)
    
  })
  
  
  ### Data and Visualizations actions ###
  
  # Data reactive
  ## load data when all required cols are selected 
  ## encode column names
  ## remove missing values from required cols
  d <- reactive({
    req(input$data)
    req(input$node_s != '')
    req(input$node_e != '')
    req(input$usubjid != '')
    req(input$pathno != '')
    
    validate(
      need(input$data, 'Data not loaded!'),
      need(input$usubjid != '', 'Please select a subject id column'),
      need(input$node_s != '', 'Please select a starting node column'),
      need(input$node_e != '', 'Please select an ending node column'),
      need(input$pathno != '', 'Please select a path number column')
    )
    
    data <- data_uploaded() 
    
    
    data2 <- data %>%
      rename('PATHNO_ENCODED' = isolate(input$pathno), 
             'NODE_S_ENCODED' = isolate(input$node_s), 
             'NODE_E_ENCODED' = isolate(input$node_e),
             'USUBJID_ENCODED' = isolate(input$usubjid))
    
    if (input$pathname != '') {
      data2 <- data2 %>%
        rename('PATHNAME_ENCODED' = isolate(input$pathname))
    }
    data2[is.na(data2$NODE_S_ENCODED),'NODE_S_ENCODED'] <- 'Missing'
    data2[is.na(data2$NODE_E_ENCODED),'NODE_E_ENCODED'] <- 'Missing'
    data2[!is.na(data2$PATHNO_ENCODED) & !is.na(data2$USUBJID_ENCODED),]
  })
  
  
  
  # add popularity percentage for filtering of links
  add_path_perc <- function(data){
    
    no_of_paths <- data$PATHNO_ENCODED %>%
      unique() %>%
      length()
    
    denom <- nrow(data)/no_of_paths
    
    nom <- data %>%
      group_by(NODE_S_ENCODED, NODE_E_ENCODED, PATHNO_ENCODED) %>%
      summarise(ctryarmcount = n()) 
    
    
    
    nom$cur_perc <- round(100*nom$ctryarmcount/denom, 1)
    
    data <- merge(data, nom, by = c('NODE_S_ENCODED', 'NODE_E_ENCODED', 'PATHNO_ENCODED'))
    
    minpercs <- data %>% 
      group_by(USUBJID_ENCODED) %>%
      summarise(minprc = min(cur_perc))
    
    data <- merge(data, minpercs, by = 'USUBJID_ENCODED')
    
    return(data)
  }

  # Handle missing data 
  missing_data <- function(data_sub){
    path_uniq <- data_sub$PATHNO_ENCODED %>%
      unique()
    path_sum <- path_uniq %>%
      sum()

    data_sub2 <- data_sub %>%
      group_by(USUBJID_ENCODED) %>%
      summarise(s2 = sum(PATHNO_ENCODED))

    subjects <- data_sub2[data_sub2$s2 != path_sum,] %>%
      pull(USUBJID_ENCODED)

    if (length(subjects) != 0){
      for (subj in subjects){
        paths <- data_sub %>%
          filter(USUBJID_ENCODED == subj) %>%
          pull(PATHNO_ENCODED) %>%
          unique()

        paths2 <- sort(path_uniq[!(path_uniq %in% paths)])
        for (path in paths2){
          if (path == min(path_uniq)){
            res <- data_sub %>%
              filter(USUBJID_ENCODED == subj & PATHNO_ENCODED == (path+1)) %>%
              mutate(NODE_E_ENCODED = NODE_S_ENCODED) %>%
              mutate(NODE_S_ENCODED = 'Missing')

          } else if (path == max(path_uniq)) {
            res <- data_sub %>%
              filter(USUBJID_ENCODED == subj & PATHNO_ENCODED == (path-1)) %>%
              mutate(NODE_S_ENCODED = NODE_E_ENCODED) %>%
              mutate(NODE_E_ENCODED = 'Missing')
          } else {
            prev <- data_sub %>%
              filter(USUBJID_ENCODED == subj & PATHNO_ENCODED == (path-1)) %>% 
              pull(NODE_E_ENCODED)

            if ((path+1) %in% paths){
              res <- data_sub %>%
                filter(USUBJID_ENCODED == subj & PATHNO_ENCODED == (path+1)) %>%
                mutate(NODE_E_ENCODED = NODE_S_ENCODED) %>%
                mutate(NODE_S_ENCODED = prev[1])
            } else {
              res <- data_sub %>%
                filter(USUBJID_ENCODED == subj & PATHNO_ENCODED == (path-1)) %>%
                mutate(NODE_S_ENCODED = NODE_E_ENCODED) %>%
                mutate(NODE_E_ENCODED = 'Missing') 

            }
            
          }
          if ('PATHNAME_ENCODED' %in% colnames(data_sub)){
              name <- data_sub %>% 
                filter(PATHNO_ENCODED == path) %>%
                pull(PATHNAME_ENCODED)
              res$PATHNAME_ENCODED <- name[1]
            }
          res <- res %>% 
            mutate(PATHNO_ENCODED = path)
          
          data_sub <- data_sub %>% 
            rbind(res)
        }
        
      }
    }

    return(data_sub)
  }
  
  # Filter Data (Path Range, Path Percentage and Filters selected)
  filter_data <- function(data){
    data_sub <- data %>%
      filter(PATHNO_ENCODED >= isolate(input$path_range[1]) & PATHNO_ENCODED <= isolate(input$path_range[2]))
    
    ## Handle missing data
    data_sub <- missing_data(data_sub)

    data_sub <- add_path_perc(data_sub)

    data_sub <- data_sub%>%
      filter(minprc >= isolate(input$perc))

    for (f in input$filters){
      filter_selected <- isolate(input[[paste0(f, '_filter')]])
      if (f %in% c('PATHNO', 'PATHNAME', 'USUBJID', 'NODE_S', 'NODE_E')){
        f <- f %>% 
          paste0('_ENCODED')
      }
      type <- class(data_sub[,f])
      if (type == 'integer'|type == 'numeric'){
        filt_expr <- paste0(f, '>=',filter_selected[1],'&',f,'<=', filter_selected[2])
        data_sub <- data_sub %>%
          filter(!! rlang::parse_expr(filt_expr))
      }else if((type == 'logical' | type == 'factor' | type == 'character') & !is.null(filter_selected)){
        
        filter_selected <- paste(filter_selected, collapse = "', '")
        
        filt_expr <- paste0(f,"%in% c('",filter_selected, "')")
        data_sub <- data_sub %>%
          filter(!! rlang::parse_expr(filt_expr))
      }
    }


    return(data_sub)
    
  }

  data_sub <- eventReactive(input$update,{
    d() %>%
      filter_data()
  })
  
  source(file.path("server", "sankey_function.R"),  local = TRUE)$value
  source(file.path("server", "linktable_function.R"),  local = TRUE)$value
  source(file.path("server", "bar_function.R"),  local = TRUE)$value
  

  # Patients included after filtering no.
  output$no_of_patients <- renderUI({
    if (input$update == 0){
      return()
    }
    
    data_sub_subjects <- data_sub() %>% 
      pull('USUBJID_ENCODED') %>% 
      unique() %>%
      length()
    
    HTML(data_sub_subjects)
  })
  

  # Time filtered for Path names in Dashboard
  output$days <- renderUI({
    if (input$update == 0){
      return()
    }
    
    validate(
      need(!(isolate(input$timepoint_labels)), '')
      )

    data_sub <- data_sub()
    
    pathname <- input$pathname
    pathno <- input$pathno
    
    if (pathname != ''){
      ## getting dates from unique paths
      dates <- data_sub %>% 
        pull(PATHNAME_ENCODED) %>%
        factor() %>%
        unique() %>% 
        droplevels() %>%
        levels()

      if (str_detect(dates[1], '-')){
        dates <- dates %>%
          paste(., collapse = '.') %>% 
          str_replace_all(.,'\\.[A-Za-z0-9]*', '')
      } else {
        dates <- dates %>%
          paste(., collapse = ' - ') 
      }
        
    } else {
      ## if PATHNO not provided
      dates <- ''
    }
    
    
    HTML(dates)
  })
  
  
  
  ## Add a slider for grouping link colors by timepoint when switching link mode
  observeEvent(c(input$mode_switch, input$path_range),
               {
                 data_sub <- data_sub()
                 
                 removeUI(
                     selector = 'div:has(> #orig_path)'
                   )
                 if (input$mode_switch){
                   insertUI(
                     selector = '#add_slider_here',
                     where = 'afterEnd',
                     ui = sliderTextInput('orig_path',
                                          label = 'Grouping Timepoint', 
                                          choices = prettyNum(min(data_sub$PATHNO_ENCODED):(max(data_sub$PATHNO_ENCODED)+1), big.mark = ","),
                                          selected = min(data_sub$PATHNO_ENCODED),
                                          grid = FALSE, dragRange = FALSE)
                   )
                 }
                 
               })

  ## Add a select for grouping link colors by NODE_S or NODE_E when switching link mode 2
  observeEvent(c(input$mode_switch2),
               {
                 
                 removeUI(
                     selector = 'div:has(> #node_s_e)'
                   )
                 if (input$mode_switch2){
                   insertUI(
                     selector = '#add_slider_here2',
                     where = 'afterEnd',
                     ui = sliderTextInput('node_s_e',
                                          label = 'Grouping Selection', 
                                          choices = prettyNum(c('NODE_S', 'NODE_E'), big.mark = ","),
                                          selected = 'NODE_S',
                                          grid = FALSE, dragRange = FALSE)
                   )
                 }
                 
               })

  ## Add a choice for order by size or name when order switch on
  observeEvent(c(input$order),
               {
                 
                 removeUI(
                     selector = 'div:has(> #order_option)'
                   )
                 if (input$order){
                   insertUI(
                     selector = '#order_option_here',
                     where = 'afterEnd',
                     ui = sliderTextInput('order_option',
                                          label = 'Order Option', 
                                          choices = prettyNum(c('Size', 'Name'), big.mark = ","),
                                          selected = 'Size',
                                          grid = FALSE, dragRange = FALSE)
                   )
                 }
                 
               })

  ## Add a slider for link opacity
  observeEvent(input$link_static_opacity,
               {
                 
                 removeUI(
                     selector = 'div:has(> #link_static_opacity_num)'
                   )
                 if (input$link_static_opacity){
                   insertUI(
                     selector = '#link_static_opacity_here',
                     where = 'afterEnd',
                     ui = sliderTextInput('link_static_opacity_num',
                                          label = 'Opacity', 
                                          choices = prettyNum((1:10)/10, big.mark = ","),
                                          selected = '0.2',
                                          grid = FALSE, dragRange = FALSE)
                   )
                 }
               })

  ## Add a slider for node opacity
  observeEvent(input$node_static_opacity,
               {
                 
                 removeUI(
                     selector = 'div:has(> #node_static_opacity_num)'
                   )
                 if (input$node_static_opacity){
                   insertUI(
                     selector = '#node_static_opacity_here',
                     where = 'afterEnd',
                     ui = sliderTextInput('node_static_opacity_num',
                                          label = 'Opacity', 
                                          choices = prettyNum((1:10)/10, big.mark = ","),
                                          selected = '0.9',
                                          grid = FALSE, dragRange = FALSE)
                   )
                 }
               })
  
  
  ## Add a slider for number of top nodes by size when switching node mode
  observeEvent(c(input$top_nodes, input$advanced_top, input$path_range),
               {

                 unfiltered <- d()
                 data_sub <- data_sub() %>% 
                   group_by(PATHNO_ENCODED) %>%
                   summarise(n = n_distinct(NODE_S_ENCODED)) 

                 overall_min <- data_sub %>%
                   pull(n) %>%
                   min()

                 removeUI(
                       selector = 'div:has(> #top_nodes_no)'
                     )

                 for (p in sort(unique(unfiltered$PATHNO_ENCODED))){
                        removeUI(
                         selector = paste0('div:has(> #top_nodes_no', p, ')')
                       )
                      }

                  removeUI(
                     selector = paste0('div:has(> #top_nodes_no', p+1, ')')
                   )



                 if (input$top_nodes){
                    if(input$advanced_top){
                      

                      last_choices <- data_sub() %>% 
                       filter(PATHNO_ENCODED == max(PATHNO_ENCODED)) %>%
                       summarise(n = n_distinct(NODE_E_ENCODED)) %>%
                       pull(n)


                      insertUI(
                         selector = '#add_top_nodes_slider',
                         where = 'afterEnd',
                         ui = sliderTextInput(paste0('top_nodes_no',max(data_sub$PATHNO_ENCODED)+1),
                                              label = paste0('Number of Top Nodes, Timepoint: ', max(data_sub$PATHNO_ENCODED)+1), 
                                              choices = prettyNum(1:(last_choices[1]), big.mark = ","),
                                              selected = 1,
                                              grid = FALSE, dragRange = FALSE)
                       )

                      for (p in sort(unique(data_sub$PATHNO_ENCODED),decreasing =TRUE)){
                        sub_min <- data_sub %>%
                          filter(PATHNO_ENCODED == p) %>%
                          pull(n)

                        insertUI(
                         selector = '#add_top_nodes_slider',
                         where = 'afterEnd',
                         ui = sliderTextInput(paste0('top_nodes_no',p),
                                              label = paste0('Number of Top Nodes, Timepoint: ', p), 
                                              choices = prettyNum(1:(sub_min[1]), big.mark = ","),
                                              selected = 1,
                                              grid = FALSE, dragRange = FALSE)
                       )
                      }
                    } else {
                      
                      insertUI(
                       selector = '#add_top_nodes_slider',
                       where = 'afterEnd',
                       ui = sliderTextInput('top_nodes_no',
                                            label = 'Number of Top Nodes', 
                                            choices = prettyNum(1:(overall_min-1), big.mark = ","),
                                            selected = 1,
                                            grid = FALSE, dragRange = FALSE)
                     )
                    }
                 }
                 
               })
  
  
  ## reactive max percentage
  observeEvent(c(input$update, input$path_range),
               {
                 
                 data <- d()
                 data_sub <- data %>%
                   filter(PATHNO_ENCODED >= isolate(input$path_range[1]) & PATHNO_ENCODED <= isolate(input$path_range[2]))


                 data_sub <- missing_data(data_sub)

                 
                 for (f in input$filters){
                   filter_selected <- isolate(input[[paste0(f, '_filter')]])
                   if (f %in% c('PATHNO', 'PATHNAME', 'USUBJID', 'NODE_S', 'NODE_E')){
                      f <- f %>% 
                        paste0('_ENCODED')
                      }
                   type <- class(data_sub[,f])
                   if (type == 'integer'|type == 'numeric'){
                     filt_expr <- paste0(f, '>=',filter_selected[1],'&',f,'<=', filter_selected[2])
                     data_sub <- data_sub %>%
                       filter(!! rlang::parse_expr(filt_expr))
                   }else if((type == 'logical' | type == 'factor' | type == 'character') & !is.null(filter_selected)){
                     
                     filter_selected <- paste(filter_selected, collapse = "', '")
                    
                     filt_expr <- paste0(f,"%in% c('",filter_selected, "')")
                     data_sub <- data_sub %>%
                       filter(!! rlang::parse_expr(filt_expr))
                   }
                 }
                 
                 data_sub <- data_sub %>%
                   add_path_perc() %>%
                   pull(minprc) %>% 
                   max()
                 
                 
                 updateSliderTextInput(session,
                                       "perc",
                                       choices = prettyNum(seq(0,data_sub,0.1), big.mark = ",")
                 )
               })


  ## Legend option
  observeEvent(input$node_unique, {

    if (isolate(input$node_unique)){
      insertUI(
                       selector = '#node_unique_after',
                       where = 'afterEnd',
                       ui = checkboxInput('legend',
                                            label = 'Add Legend'
                                            )
                     )
    } else {
      removeUI(
        selector = 'div:has(> div:has(> label:has(> #legend)))'
        )
    }


  })

  ## legend styles
  observeEvent(input$legend, {

    removeUI(
      selector = 'div:has(> #legend_title)'
      )

    removeUI(
      selector = 'div:has(> #legend_font_size)'
      )

    removeUI(
      selector = 'div:has(> #legend_font)'
      )

    removeUI(
      selector = 'div:has(> #legend_x)'
      )

    removeUI(
      selector = 'div:has(> #legend_nrow)'
      )


    if (input$legend){
      insertUI(
               selector = '#legend_style_here',
               where = 'afterEnd',
               ui = sliderTextInput('legend_x',
                                    label = 'x coordinate', 
                                    choices = prettyNum(-100:100, big.mark = ","),
                                    selected = '0',
                                    grid = FALSE, dragRange = FALSE)
               )

      insertUI(
               selector = '#legend_style_here',
               where = 'afterEnd',
               ui = sliderTextInput('legend_nrow',
                                    label = 'Number per column', 
                                    choices = prettyNum(1:10, big.mark = ","),
                                    selected = '4',
                                    grid = FALSE, dragRange = FALSE)
               )

      insertUI(
               selector = '#legend_style_here',
               where = 'afterEnd',
               ui = sliderTextInput('legend_font_size',
                                    label = 'Text Size', 
                                    choices = prettyNum(1:30, big.mark = ","),
                                    selected = '15',
                                    grid = FALSE, dragRange = FALSE)
               )

      insertUI(
               selector = '#legend_style_here',
               where = 'afterEnd',
               ui = textInput("legend_font",
                                 label = "Text Font",
                                 value = "")
               )

      insertUI(
               selector = '#legend_style_here',
               where = 'afterEnd',
               ui = textInput("legend_title",
                               label = "Title",
                               value = "")
               )



    
    }


  })

  ## timepoint styles
  observeEvent(input$timepoint_labels, {

    removeUI(
      selector = 'div:has(> #timepoints_font_size)'
      )

    removeUI(
      selector = 'div:has(> #timepoints_font)'
      )

    removeUI(
      selector = 'div:has(> #timepoints_x)'
      )

    if (input$timepoint_labels){

      insertUI(
               selector = '#timepoint_style_here',
               where = 'afterEnd',
               ui = sliderTextInput('timepoints_x',
                                    label = 'x coordinate', 
                                    choices = prettyNum(-50:50, big.mark = ","),
                                    selected = '-5',
                                    grid = FALSE, dragRange = FALSE)
               )

      insertUI(
               selector = '#timepoint_style_here',
               where = 'afterEnd',
               ui = sliderTextInput('timepoints_font_size',
                                    label = 'Text Size', 
                                    choices = prettyNum((1:50)/10, big.mark = ","),
                                    selected = '1',
                                    grid = FALSE, dragRange = FALSE)
               )

      insertUI(
               selector = '#timepoint_style_here',
               where = 'afterEnd',
               ui = textInput("timepoints_font",
                                 label = "Text Font",
                                 value = "")
               )

    }


  })

  ## General Title Options
  observeEvent(input$general_title, {

    removeUI(
      selector = 'div:has(> #general_title_font_size)'
      )

    removeUI(
      selector = 'div:has(> #general_title_font)'
      )

    removeUI(
      selector = 'div:has(> #general_title_x)'
      )

    removeUI(
      selector = 'div:has(> #general_title_text)'
      )

    if (input$general_title){

      insertUI(
               selector = '#general_title_here',
               where = 'afterEnd',
               ui = sliderTextInput('general_title_x',
                                    label = 'x coordinate', 
                                    choices = prettyNum(-100:100, big.mark = ","),
                                    selected = '0',
                                    grid = FALSE, dragRange = FALSE)
               )

      insertUI(
               selector = '#general_title_here',
               where = 'afterEnd',
               ui = sliderTextInput('general_title_font_size',
                                    label = 'Text Size', 
                                    choices = prettyNum((1:50)/10, big.mark = ","),
                                    selected = '2.5',
                                    grid = FALSE, dragRange = FALSE)
               )

      insertUI(
               selector = '#general_title_here',
               where = 'afterEnd',
               ui = textInput("general_title_font",
                                 label = "Text Font",
                                 value = "")
               )

      insertUI(
               selector = '#general_title_here',
               where = 'afterEnd',
               ui = textInput("general_title_text",
                                 label = "Title Text",
                                 value = "")
               )

    }


  })

  ## General Footnote Options
  observeEvent(input$general_footnote, {

    removeUI(
      selector = 'div:has(> #general_footnote_font_size)'
      )

    removeUI(
      selector = 'div:has(> #general_footnote_font)'
      )

    removeUI(
      selector = 'div:has(> #general_footnote_text)'
      )

    if (input$general_footnote){

      insertUI(
               selector = '#general_footnote_here',
               where = 'afterEnd',
               ui = sliderTextInput('general_footnote_font_size',
                                    label = 'Text Size', 
                                    choices = prettyNum((1:50)/10, big.mark = ","),
                                    selected = '1',
                                    grid = FALSE, dragRange = FALSE)
               )

      insertUI(
               selector = '#general_footnote_here',
               where = 'afterEnd',
               ui = textInput("general_footnote_font",
                                 label = "Text Font",
                                 value = "")
               )

      insertUI(
               selector = '#general_footnote_here',
               where = 'afterEnd',
               ui = textInput("general_footnote_text",
                                 label = "Footnote Text",
                                 value = "")
               )

    }


  })

  ## Prompt Instructions
  observeEvent(input$manual_colors, {

    removeUI(
      selector = '#explanation'
      )

    if (isolate(input$manual_colors)){

      insertUI(
               selector = '#prompt_explanation',
               where = 'afterEnd',
               ui = tags$div(
                HTML('<p>
                          Node Colorings:<br>  
                          <ul>
                            <li> <b>[color]</b> : Color the node clicked</li>
                            <li> <b>group:[color]</b> : Color all nodes with same name</li> 
                            <li> <b>link:[node_s/node_e/origin]:[color]</b> : Color all links with starting node, ending node or origin, respectively
                            same as the node clicked</li>
                          </ul>
                          <br>
                          Link Colorings:<br>
                          <ul>
                            <li> <b>[color]</b> : Color the link clicked</li>
                          </ul>
                          <br>
                          Colors can be names of colors, RGB or hex colors.
                          </p>'), 
                id = "explanation")
               )
    }


  })
  
  
  
  
  ## download sankey in HTML
  output$download <- downloadHandler(
    filename = function() {
      paste('sankey-network-', Sys.Date(), '.html', sep='')
    },
    content = function(file){
      sankey_gen(html = TRUE) %>% saveNetwork(file)
    }
  )


  output$download_wide <- downloadHandler(
    filename = function() {
      return('temp_wide.R')
    },
    content = function(file){
      filename <- paste0(myDir, '/temp_wide.R')
      file.copy(filename, file)
    }
  )

  output$download_long <- downloadHandler(
    filename = function() {
      return('temp_long.R')
    },
    content = function(file){
      filename <- paste0(myDir, '/temp_long.R')
      file.copy(filename, file)
    }
  )
  
}

