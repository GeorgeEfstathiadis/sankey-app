tableDataGen <- function(endpoint){
	if (input$update == 0){
      return()
    }
    
    
    data_sub <- d() %>%
      filter_data()


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
      
      ## Encoding nodes
      if (i==1){
        data_p <- data_p %>%
          merge(nodedata[1:total_no_list[1],], by.x = "NODE_S_ENCODED", by.y = "name", all = TRUE) %>%
          merge(nodedata[(total_no_list[1]+1):total_no_list[2],], by.x = "NODE_E_ENCODED", by.y = "name", all =TRUE)
        
        links <- data_p
      }
      else {
        data_p <- data_p %>%
          merge(nodedata[(total_no_list[i-1]+1):total_no_list[i],], by.x = "NODE_S_ENCODED", by.y = "name", all = TRUE) %>%
          merge(nodedata[(total_no_list[i]+1):total_no_list[i+1],], by.x = "NODE_E_ENCODED", by.y = "name", all =TRUE)
        
        links <- rbind(links, data_p)
      }
    }
    
    
    links <- links[order(links$PATHNO_ENCODED, links$value),]

    nodes <- links %>%
    	group_by(PATHNO_ENCODED, NODE_S_ENCODED) %>%
    	summarise(size = sum(value))

    colnames(nodes) <- c('Timepoint', 'Node', 'Size')

    nodes_e <- links %>%
    	filter(PATHNO_ENCODED == max(paths)) %>%
    	group_by(NODE_E_ENCODED) %>%
    	summarise(size = sum(value)) %>%
    	cbind(max(paths))

    nodes_e <- nodes_e[,c(3,1,2)]
    colnames(nodes_e) <- c('Timepoint', 'Node', 'Size')
    nodes <- rbind(nodes, nodes_e)

    if (endpoint == 'link'){
    	return(links)
    } else if (endpoint == 'node') {
    	return(nodes)
    }
}

# Table for Link Size text
output$LinkSizeTable <- renderDataTable({
	links <- tableDataGen('link') %>%
    	select(PATHNO_ENCODED, ORIGIN, NODE_S_ENCODED, NODE_E_ENCODED, value)

    colnames(links) <- c('Path Number', 'Origin Node', 'Start Node', 'End Node', 'Size')

    DT::datatable(links, 
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
                    width = "100%",
                    class = 'cell-border stripe'
                  )
    )
})

# Table for Node Size text
output$NodeSizeTable <- renderDataTable({
	nodes <- tableDataGen('node') 

    DT::datatable(nodes, 
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
                    width = "100%",
                    class = 'cell-border stripe'
                  )
    )
})