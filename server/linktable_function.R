tableDataGen <- function(endpoint){
    if (input$update == 0){
      return()
    }
    
    
    data_sub <- data_sub()


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
    
    nodes <- links %>%
    	group_by(PATHNO_ENCODED, NODE_S_ENCODED) %>%
    	summarise(size = sum(value))

    colnames(nodes) <- c('Timepoint', 'Node', 'Size')

    nodes_e <- links %>%
    	filter(PATHNO_ENCODED == max(paths)) %>%
    	group_by(NODE_E_ENCODED) %>%
    	summarise(size = sum(value)) %>%
    	cbind(max(paths)+1)

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
	validate(
      need(input$data, 'No data loaded')
    )
    validate(
      need(input$update != 0, 'No data loaded')
    )

	links <- tableDataGen('link') %>%
  	select(PATHNO_ENCODED, ORIGIN, NODE_S_ENCODED, NODE_E_ENCODED, value)
  
  if (isolate(input$remove_missing)){
    links <- links %>%
      filter(NODE_S_ENCODED != 'Missing' & NODE_E_ENCODED != 'Missing')
  }
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
	validate(
      need(input$data, 'No data loaded')
    )
    validate(
      need(input$update != 0, 'No data loaded')
    )
	nodes <- tableDataGen('node') 

  if (isolate(input$remove_missing)){
    nodes <- nodes %>%
      filter(Node != 'Missing')
  }

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