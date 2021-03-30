# Sankey network creation
  sankey_gen <- function(html = FALSE){
    
    if (input$update == 0){
      return()
    }
    
    
    data_sub <- data_sub()


    ## Pathname config
    pathname <- input$pathname
    
    if (isolate(input$timepoint_labels)){
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
            str_replace_all(.,'\\.[A-Za-z0-9]*', '') %>%
            str_split(' - ')
        }  else {
          date <- ''

          showNotification('Not correct Pathname format, check help page example.', type = 'warning')
        }
    } else {
      dates <- ''
      showNotification('No Pathname column provided.', type = 'warning')
    }
  }
    

    # nodedata
    paths <- data_sub$PATHNO_ENCODED %>% 
      unique() %>% 
      sort()
    
    total_vec <- c()
    total_no_list <- c()
    paths_vec <- c()
    
    ## Get all starting nodes from each path
    for (p in paths){
      nodes_p <- paste(unique(data_sub%>%
                                filter(PATHNO_ENCODED==p)%>%
                                pull(NODE_S_ENCODED)
                                )
      )
      
      nodes_p <- nodes_p[order(nodes_p)]
      
      total_vec <- total_vec %>%
        append(nodes_p)
      
      total_no_list <- total_no_list %>%
        append(length(total_vec))
      
      paths_vec <- paths_vec %>%
        append(rep(p, length(nodes_p)))
    }
    
    ## Get the final layer of nodes by the end nodes of the final path
    nodes_p <- paste(unique(data_sub%>%
                              filter(PATHNO_ENCODED==p)%>%
                              pull(NODE_E_ENCODED)
                              )
    )
    
    nodes_p <- nodes_p[order(nodes_p)]
    
    total_vec <- total_vec %>%
      append(nodes_p)
    
    total_no_list <- total_no_list %>%
      append(length(total_vec))
    
    paths_vec <- paths_vec %>%
      append(rep(p+1, length(nodes_p)))
    
    
    comb<-length(total_vec)
    nodedata<-data.frame(node = c(0:(comb-1)), name = total_vec,stringsAsFactors = FALSE) 
    timepoints <- c()
    for (i in 1:length(paths)){
      if (i == 1){
        timepoints <- c(timepoints,rep(paths[i],total_no_list[i]))
      } else {
        timepoints <- c(timepoints,rep(paths[i],total_no_list[i]-total_no_list[i-1]))
      }
      
    }
    timepoints <- c(timepoints,rep(paths[i]+1,total_no_list[i+1]-total_no_list[i]))
  
    nodedata$timepoint <- timepoints
    
    # origin node
    origins <- data_sub %>%
      filter(PATHNO_ENCODED == paths[1]) %>%
      select(USUBJID_ENCODED, NODE_S_ENCODED) %>% 
      rename(ORIGIN = NODE_S_ENCODED)
    
    if (isolate(input$mode_switch)){
      if (isolate(input$orig_path) %in% paths){
        origins <- data_sub %>%
          filter(PATHNO_ENCODED == isolate(input$orig_path)) %>%
          select(USUBJID_ENCODED, NODE_S_ENCODED) %>% 
          rename(ORIGIN = NODE_S_ENCODED)
      } else {
        origins <- data_sub %>%
          filter(PATHNO_ENCODED == (isolate(input$orig_path)-1)) %>%
          select(USUBJID_ENCODED, NODE_E_ENCODED) %>% 
          rename(ORIGIN = NODE_E_ENCODED)
      }
    }
    
    data_sub <- data_sub %>%
      merge(., origins, by = 'USUBJID_ENCODED')
    
    # links
    no_of_paths2 = length(paths)
    
    for (i in 1:no_of_paths2){
      ## Gathering links and number for each path
      data_p <- data_sub %>% 
        filter(PATHNO_ENCODED == paths[i]) %>%
        group_by(NODE_S_ENCODED, NODE_E_ENCODED, ORIGIN) %>%
        summarise(value = n(), PATHNO_ENCODED = mean(PATHNO_ENCODED)) 
      
      nodes_s <- nodedata %>%
        filter(timepoint == paths[i])

      nodes_e <- nodedata %>%
        filter(timepoint == (paths[i]+1))
      ## Encoding nodes
      data_p <- data_p %>%
        merge(nodes_s, by.x = "NODE_S_ENCODED", by.y = "name", all = TRUE) %>%
        merge(nodes_e, by.x = "NODE_E_ENCODED", by.y = "name", all =TRUE)
      
      if (i==1){
        links <- data_p
      } else {
        links <- rbind(links, data_p)
      }
    }
    
    
    links <- links[order(links$PATHNO_ENCODED, links$value),] 
    
    # Ordering
    no_nodes = length(nodedata$node)
    
    ## will contain all the nodes for each path ordered by path and then by size
    nodedata_ord <- nodedata[0,]
    
    for (i in 1:no_of_paths2){
      ## getting the size of each node
      path <- links %>% 
        filter(PATHNO_ENCODED == paths[i]) %>%
        group_by(NODE_S_ENCODED) %>%
        summarise(size = sum(value))  %>%
        rename(name = NODE_S_ENCODED)
      
      
      nodedata_1 <- NULL
      
      nodedata_1 <- nodedata %>%
        filter(timepoint == paths[i]) %>%
        merge(path, by = 'name') 
      
      ## order them by size
      nodedata_1 <- nodedata_1[order(nodedata_1$size, nodedata_1$name),c(2,1,4,3)] 
      ## group nodes by size
      if (isolate(input$top_nodes)){
        if (isolate(input$advanced_top)){
          top_nodes <- isolate(input[[paste0('top_nodes_no', paths[i])]])
        } else {
          top_nodes <- isolate(input$top_nodes_no)
        }

        if (top_nodes < nrow(nodedata_1)){
          nodedata_1_top <- nodedata_1[(nrow(nodedata_1)-top_nodes + 1):nrow(nodedata_1),] 
          nodedata_1_bottom <- nodedata_1[1:(nrow(nodedata_1)-top_nodes),] 
          id <- nodedata_1_bottom[1, 1]
          size_other <- nodedata_1_bottom$size %>% sum()
          nodedata_1 <- rbind(c(id, 'Other', size_other, paths[i]), nodedata_1_top)
          }
      }
      

      nodedata_ord <- rbind(nodedata_ord, nodedata_1)
    }
    ## final layer - same process
    path <- links %>% 
      filter(PATHNO_ENCODED == paths[no_of_paths2]) %>%
      group_by(NODE_E_ENCODED) %>%
      summarise(size = sum(value))  %>%
      rename(name = NODE_E_ENCODED)
    
    nodedata_1 <- NULL
    
    nodedata_1 <- nodedata %>%
      filter(timepoint == paths[i]+1) %>%
      merge(path, by = 'name') 
    
    nodedata_1 <- nodedata_1[order(nodedata_1$size, nodedata_1$name),c(2,1,4,3)] 

    if (isolate(input$top_nodes)){
      if (isolate(input$advanced_top)){
        top_nodes <- isolate(input[[paste0('top_nodes_no', paths[i]+1)]])
      }
      if (top_nodes < nrow(nodedata_1)){
        nodedata_1_top <- nodedata_1[(nrow(nodedata_1)-top_nodes + 1):nrow(nodedata_1),] 
        nodedata_1_bottom <- nodedata_1[1:(nrow(nodedata_1)-top_nodes),] 
        id <- nodedata_1_bottom[1, 1]
        size_other <- nodedata_1_bottom$size %>% sum()
        nodedata_1 <- rbind(c(id, 'Other', size_other, paths[i]+1), nodedata_1_top)
      }
    }
    
    nodedata_ord <- rbind(nodedata_ord, nodedata_1)
    
    row.names(nodedata_ord) <- NULL
    no_nodes <- nrow(nodedata_ord)

    ## Order Nodedata by name
    if (isolate(input$order)){
      if (isolate(input$order_option == 'Name')){
        nodedata_ord <- nodedata_ord[order(nodedata_ord$timepoint, nodedata_ord$name),] 
      }
    }
    nodedata_ord$node_ord <- (1:no_nodes-1)
    
    ## Redo the link creation but with known order
    if (isolate(input$top_nodes)){
      grouped_nodes_or <- nodedata_ord %>%
        filter(timepoint == min(timepoint)) %>%
        pull(name)  
      
      if (isolate(input$mode_switch)){
        selected_timepoint <- isolate(input$orig_path)
        grouped_nodes_or <- nodedata_ord %>% 
          filter(timepoint == selected_timepoint) %>%
          pull(name) 
      }
    }
    
    
    for (i in 1:no_of_paths2){
      data_p <- data_sub %>% 
        filter(PATHNO_ENCODED == paths[i]) %>%
        group_by(NODE_S_ENCODED, NODE_E_ENCODED, ORIGIN) %>%
        summarise(value = n(), PATHNO_ENCODED = mean(PATHNO_ENCODED)) 
      
      if (isolate(input$top_nodes)){
        nodes_s <- nodedata_ord %>% 
          filter(timepoint == paths[i])
        
        nodes_e <- nodedata_ord %>% 
          filter(timepoint == (paths[i]+1))
        
        grouped_nodes_s <- nodes_s %>%
          pull(name)
        grouped_nodes_e <- nodes_e %>%
          pull(name)
        
        
        data_p[!(data_p$NODE_S_ENCODED %in% grouped_nodes_s), 'NODE_S_ENCODED'] <- 'Other'
        data_p[!(data_p$NODE_E_ENCODED %in% grouped_nodes_e), 'NODE_E_ENCODED'] <- 'Other'
        
        data_p[!(data_p$ORIGIN %in% grouped_nodes_or), 'ORIGIN'] <- 'Other' 
        
        data_p <- data_p %>%
          group_by(NODE_S_ENCODED, NODE_E_ENCODED, ORIGIN) %>%
          summarise(value = sum(value), PATHNO_ENCODED = mean(PATHNO_ENCODED))%>%
          merge(nodes_s, by.x = "NODE_S_ENCODED", by.y = "name", all = TRUE) %>%
          merge(nodes_e, by.x = "NODE_E_ENCODED", by.y = "name", all =TRUE)
        
        if (i==1){
          links <- data_p
        }
        else {
          links <- rbind(links, data_p)
        }
      } else {
        nodes_s <- nodedata_ord %>% 
          filter(timepoint == paths[i])
        
        nodes_e <- nodedata_ord %>% 
          filter(timepoint == (paths[i]+1))
        
        data_p <- data_p %>%
          merge(nodes_s, by.x = "NODE_S_ENCODED", by.y = "name", all = TRUE) %>%
          merge(nodes_e, by.x = "NODE_E_ENCODED", by.y = "name", all =TRUE)
        
        if (i==1){
          links <- data_p
        }
        else {
          links <- rbind(links, data_p)
        }
      }
      
    }
    
    
    links <- links[order(links$PATHNO_ENCODED, links$value),] 
    if (isolate(input$order)){
      if (isolate(input$order_option == 'Name')){
        links <- links[order(links$PATHNO_ENCODED, links$NODE_S_ENCODED, links$NODE_E_ENCODED),]
      }
    }
    
    ## if iteration 0 sankey keeps the order given to it
    ## otherwise it finds the one that makes the links the most visible
    if (isolate(input$order)){
      iterations = 0
    }
    else {
      iterations = 32
    }
    
    # grouping color 
    treatment <- c()
    ## grouping colours
    rand_colors <- brewer.pal(12, "Paired") %>%
      append(brewer.pal(8, "Dark2")) %>%
      append(brewer.pal(11, "Spectral")) %>%
      append(brewer.pal(11, "PRGn")) %>%
      append(brewer.pal(11, "BrBG")) %>%
      append(brewer.pal(11, "RdGy")) 

    ### Color template 
    ### d3.scaleOrdinal().domain([{node_groups}, {link_groups}]).range([{node_colours}, {link_colours}])
    ## groupings for nodes

    if (!isolate(input$node_unique)){
      for (x in nodedata_ord$name){
        if (regexpr(isolate(input$color), x, ignore.case = TRUE)[[1]][1] != -1){
          treatment <- treatment %>% append("1")
        }
        else if (regexpr("(Disc. ?Study)|(Discontinued ?Study)", x, ignore.case = TRUE)[[1]][1] != -1){
          treatment <- treatment %>% append("3")
        }
        else if (regexpr("(No ?Trt.?)|(No ?Treatment)|(None)|(Other)|([Mm]issing)", x, ignore.case = TRUE)[[1]][1] != -1){
          treatment <- treatment %>% append("4")
        }
        else{
          treatment <- treatment %>% append("2")
        }
      }
    
    
      nodedata_ord$group <- treatment
      
      node_groups <- paste0("'",1:4,"'") %>% 
        paste(collapse=',')

      node_colours <- paste0("'",c(isolate(input$color_1),isolate(input$color_2),isolate(input$color_3),isolate(input$color_4)),"'") %>% 
        paste(collapse=',')

    } else {
      nodedata_ord$group <- nodedata_ord$name %>%
        str_replace_all(' ', '_')

      node_groups2 <- nodedata_ord$group %>%
        unique() %>%
        str_replace_all(' ', '_')
      node_groups <- paste0("'",node_groups2,"'") %>% 
        paste(collapse=',')

      node_colours <- paste0("'", rand_colors[1:length(node_groups2)],"'") %>% 
        paste(collapse=',')
    }
    
    
    
   
    
    if (isolate(input$mode_switch)){
      link_group <- "ORIGIN2"     
      links$ORIGIN2 <- links$ORIGIN %>%
        str_replace_all(' ', '_')
      
      my_groups <- links$ORIGIN2 %>%
        unique() 
      
      link_colours <- rand_colors[1:length(my_groups)] %>% 
        paste(., collapse = "','")
      
      link_groups <- my_groups %>%
        paste(., collapse = "','")

      link_colours <- paste0("'", link_colours, "'")
      link_groups <- paste0("'", link_groups, "'")

    }else if (isolate(input$mode_switch2)){
      link_group <- paste0(isolate(input$node_s_e),'_ENCODED2')
      links[,link_group] <- links[,paste0(isolate(input$node_s_e),'_ENCODED')] %>%
        str_replace_all(' ', '_')
      
      my_groups <- links[,link_group] %>%
        unique() 
      
      link_colours <- rand_colors[1:length(my_groups)] %>% 
        paste(., collapse = "','")
      
      link_groups <- my_groups %>%
        paste(., collapse = "','")

      link_colours <- paste0("'", link_colours, "'")
      link_groups <- paste0("'", link_groups, "'")

    }else{
      
      if (isolate(input$link_group)!="None" & isolate(input$link_group)!="none" ){
        link_col <- c()
        t_node_s <- grepl(isolate(input$link_group), links$NODE_S_ENCODED, ignore.case = TRUE)
        t_node_e <- grepl(isolate(input$link_group), links$NODE_E_ENCODED, ignore.case = TRUE)
        for (x in 1:length(links$NODE_S_ENCODED)){
          if (t_node_s[x] | t_node_e[x]){
            link_col <- link_col%>%append("a")
          }
          else{
            link_col <- link_col%>%append("b")
          }
        }
        links$color <-link_col

        link_groups <- paste0("'",c('a', 'b'),"'") %>% 
          paste(collapse=',')

        link_colours <- paste0("'", c('#ff4500','rgb(0,0,0)'),"'") %>% 
          paste(collapse=',')

        if (html){
          link_colours <- paste0("'", c('#ff4500','rgb(0,0,0,.2)'),"'") %>% 
            paste(collapse=',')
        }
        link_group <- "color"                  
      } else {
        link_group <- NULL

        link_groups <- paste0("'",c('a', 'b'),"'") %>% 
          paste(collapse=',')

        link_colours <- paste0("'", c('#ff4500','rgb(0,0,0)'),"'") %>% 
          paste(collapse=',')
      }
    }
    
    my_color <- paste0('d3.scaleOrdinal().domain([',node_groups,', ',link_groups,']).range([',node_colours,', ',link_colours,'])')
    margin = list(top = isolate(input$margin_top),
      bottom = isolate(input$margin_bottom),
      left = isolate(input$margin_left),
      right = isolate(input$margin_right))
    #Sankey
    sankey <- sankeyNetwork(Links = links, Nodes = nodedata_ord,
                            Source = "node_ord.x", Target = "node_ord.y",
                            Value = "value", NodeID = "name", LinkGroup = link_group, NodeGroup = "group",
                            colourScale = my_color, margin = margin,
                            fontSize = isolate(input$node_font_size), nodeWidth = isolate(input$node_width),nodePadding = isolate(input$node_padding),
                            fontFamily = isolate(input$node_font),
                            sinksRight = FALSE, height = 600, width = 800, iterations = iterations) 
    
    sankey$x$links$ORIGIN <- links$ORIGIN
    sankey$x$links$PATHNO_ENCODED <- links$PATHNO_ENCODED
    sankey$x$nodes$TIMEPOINT <- nodedata_ord$timepoint
    ## tooltip rendering with JS 
    ### along with SVG download and origin tracking
    sankey_js <- "www/JS/sankey.js"
    js_code <- readChar(sankey_js, file.info(sankey_js)$size)

    # Colour prompt 
    if (isolate(input$manual_colors)){
      js_code <- js_code %>%
        str_replace('powerBI = true', 'powerBI = false') %>%
        str_replace('manual_colors', 'manual_colors = true')
    }

    # Title - footnote
    if (isolate(input$general_title)){
      if (isolate(input$general_title_text) != ''){
        js_code <- js_code %>%
          str_replace('title = 1', paste0("title = '", isolate(input$general_title_text), "'")) %>%
          str_replace('title_font', paste0("title_font = '", isolate(input$general_title_font), "'")) %>%
          str_replace('title_size', paste0("title_size = ", isolate(input$general_title_font_size))) %>%
          str_replace('title_x', paste0("title_x = ", isolate(input$general_title_x)))
      }
    }

    if (isolate(input$general_footnote)){
      if (isolate(input$general_footnote_text) != ''){
        js_code <- js_code %>%
          str_replace('footnote = 1', paste0("footnote = '", isolate(input$general_footnote_text), "'")) %>%
          str_replace('footnote_font', paste0("footnote_font = '", isolate(input$general_footnote_font), "'")) %>%
          str_replace('footnote_size', paste0("footnote_size = ", isolate(input$general_footnote_font_size))) 
      }
    }
    
    
    # Margins 
    js_code <- js_code %>%
      str_replace('margin_top', paste0('margin_top = ', isolate(input$margin_top))) %>%
      str_replace('margin_left', paste0('margin_left = ', isolate(input$margin_left))) %>%
      str_replace('margin_right', paste0('margin_right = ', isolate(input$margin_right))) %>%
      str_replace('margin_bottom', paste0('margin_bottom = ', isolate(input$margin_bottom))) 

    # Switch js_code if link mode switch is on
    if (isolate(input$link_static_opacity)){
      js_code <- js_code %>%
        str_replace('powerBI = true', 'powerBI = false') %>% 
        str_replace('//g', paste0("link.style('stroke-opacity', ", isolate(input$link_static_opacity_num), ")"))
    } else if (isolate(input$mode_switch)){
      
    } else if (isolate(input$mode_switch2)){
      js_code <- js_code %>%
        str_replace('0.901', '0.5')
    } else {
      js_code <- js_code %>%
        str_replace('0.901', '0.2') %>%
        str_replace('//c', "d3.select(this) .style('stroke-opacity', 0.5);") %>%
        str_replace('//d', "d3.select(this) .style('stroke-opacity', 0.2);") %>%
        str_replace('//e', "if(d.x == 0){link.style('stroke-opacity', l => {return l.ORIGIN == d.name ? 0.5 : 0.2;})}") %>%
        str_replace('//f', "link.style('stroke-opacity', 0.2)") 

      if (!isolate(input$manual_colors)){
        js_code <- js_code %>%
        str_replace('//b', "d3.select(this).select('rect').style('fill', 'red');") %>%
        str_replace('//b2', "d3.select(this).select('rect').style('fill', fill);")
      }
    }

    # Node Opacity
    if (isolate(input$node_static_opacity)){
      js_code <- js_code %>%
        str_replace('powerBI = true', 'powerBI = false') %>% 
        str_replace('//h', paste0("node.select('rect').style('opacity', ", isolate(input$node_static_opacity_num), ")"))
    }

    
    ## Show node sizes
    if (isolate(input$node_show)){
      js_code <- js_code %>% 
        str_replace('units = 1', paste0("units = '", isolate(input$node_units), "'")) %>%
        str_replace('//a', "d3.selectAll('.node').select('text').style('font-weight', 'bold').text(d => d.name + ': ' + d.value + units);")
    }

    ## Hide node labels
    if (isolate(input$remove_labels)){
      js_code <- js_code %>% 
        str_replace('nodeHide = false', "nodeHide = true")
    }

    ## Remove Missing
    if (isolate(input$remove_missing)){
      js_code <- js_code %>% 
        str_replace('missing = false', "missing = true")
    }

    ## Timepoints on graph
    if (isolate(input$timepoint_labels)){
      if (dates != ''){
        dates <- dates %>%
         paste0("'",.,"'") %>% 
         paste(., collapse=',') %>%
         paste0("[",.,"]") %>%
         str_replace("'c\\(", '') %>%
         str_replace("\\)'", '')

        js_code <- js_code %>% 
          str_replace('0.0001', dates) %>%
          str_replace('1vw', paste0(isolate(input$timepoints_font_size), 'vw')) %>%
          str_replace('xcoord', paste0('xcoord = ', isolate(input$timepoints_x)))

        if (isolate(input$timepoints_font) != ''){
          js_code <- js_code %>%
            str_replace('//i', paste0(".attr('font-family', '", isolate(input$timepoints_font), "')"))
        }

      }
      
    }

    ## Legend
    if (isolate(input$node_unique)){
      if (isolate(input$legend)){
        js_code <- js_code %>%
          str_replace('legend_bool = false', 'legend_bool = true') %>%
          str_replace('legend_size', paste0('legend_size = ', isolate(input$legend_font_size))) %>%
          str_replace('legend_font', paste0("legend_font = '", isolate(input$legend_font), "'")) %>%
          str_replace('legend_nrow', paste0('legend_nrow = ', isolate(input$legend_nrow))) %>%
          str_replace('legend_x', paste0('legend_x = ', isolate(input$legend_x))) %>%
          str_replace('legend_title', paste0("legend_title = '", isolate(input$legend_title), "'"))
      }
    } 


    sankey <- onRender(sankey,js_code)
    
    return(sankey)
  }
  
  
  # Sankey Rendering
  output$SankeyPlot <- renderSankeyNetwork({
    sankey_gen()
  })