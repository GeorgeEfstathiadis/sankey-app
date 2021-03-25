# Highchart Barplot Generation
  high_bar_gen <- function(nodes, data_sub){
    if (nodes == 'start'){
      # Starting nodes barplot
      data_start_nodes <- data_sub%>%
        filter(PATHNO_ENCODED==min(PATHNO_ENCODED))%>%
        group_by(NODE_S_ENCODED)%>%
        summarise(size = n())
      
      max_start <- max(data_start_nodes$size)
      
      data_end_nodes <- data_sub%>%
        filter(PATHNO_ENCODED==max(PATHNO_ENCODED))%>%
        group_by(NODE_E_ENCODED)%>%
        summarise(size = n())
      
      max_end <- max(data_end_nodes$size)
      
      data_start_nodes <- data_start_nodes[order(-data_start_nodes$size, rev(data_start_nodes$NODE_S_ENCODED)),]
      
      ## in order for both barplots to have common y-axis size
      max_ovr <- max(c(max_start, max_end)) + 1
      
      ## percentage calculation
      data_start_nodes$size_all <- round(data_start_nodes$size / sum(data_start_nodes$size) * 100,2)
      
      ## top_nodes
      if (isolate(input$top_nodes)){
        if (isolate(input$advanced_top)){
          i <- min(data_sub$PATHNO_ENCODED)
          top_nodes <- isolate(input[[paste0('top_nodes_no', i)]])
        } else {
          top_nodes <- isolate(input$top_nodes_no)
        }

        if (top_nodes < nrow(data_start_nodes)){
          data_start_nodes_top <- data_start_nodes[1:top_nodes,]
          data_start_nodes_bottom <- data_start_nodes[(top_nodes+1):nrow(data_start_nodes),]  
          size_other <- data_start_nodes_bottom$size %>% sum()
          size_all_other <- data_start_nodes_bottom$size_all %>% sum()
          data_start_nodes <- rbind(data_start_nodes_top, c('Other', size_other,size_all_other))  
          data_start_nodes$size <- data_start_nodes$size %>% sapply(as.integer)
          data_start_nodes$size_all <- data_start_nodes$size_all %>% sapply(as.double)
          data_start_nodes <- data_start_nodes[order(-data_start_nodes$size),]
        }
        
      }

      ## Remove Missing
      if (isolate(input$remove_missing)){
        data_start_nodes <- data_start_nodes %>%
          filter(NODE_S_ENCODED != 'Missing')
      }
      
      ## actual highchart barplot
      data_start_nodes %>%  
        hchart('column', hcaes(x=NODE_S_ENCODED, y=size, color = size)) %>%
        hc_title(text = "Start Nodes",
                 style = list(fontWeight = "bold", fontSize = "30px"),
                 align = "center") %>% 
        hc_yAxis(title = list(text = "Number of Patients"),
                 opposite = FALSE,
                 labels = list(format = "{size}"),
                 max = max_ovr)%>%
        hc_xAxis(title = list(text = NULL),
                 labels = list(rotation = 0,
                               style = list(whiteSpace = "nowrap",
                                            textOverflow = "ellipsis"))) %>%
        hc_tooltip(pointFormat = "Size: {point.y} <br> Percentage: {point.size_all}%",
                   headerFormat = '<span style="font-size: 20px">{point.key}</span><br/>',
                   style = list(fontSize = "20px")) %>%
        hc_plotOptions(column = list(
          dataLabels = list(enabled = TRUE)
        ))
    }else if(nodes == 'end'){
      ## ending nodes barplot 
      data_end_nodes <- data_sub%>%
        filter(PATHNO_ENCODED==max(PATHNO_ENCODED))%>%
        group_by(NODE_E_ENCODED)%>%
        summarise(size = n())
      
      max_end <- max(data_end_nodes$size)
      
      data_start_nodes <- data_sub%>%
        filter(PATHNO_ENCODED==min(PATHNO_ENCODED))%>%
        group_by(NODE_S_ENCODED)%>%
        summarise(size = n())
      
      max_start <- max(data_start_nodes$size)
      
      data_end_nodes <- data_end_nodes[order(-data_end_nodes$size, rev(data_end_nodes$NODE_E_ENCODED)),]
      
      max_ovr <- max(c(max_start, max_end)) + 1
      
      data_end_nodes$size_all <- round(data_end_nodes$size / sum(data_end_nodes$size) * 100,2)

      ## top_nodes
      if (isolate(input$top_nodes)){
        if (isolate(input$advanced_top)){
          i <- max(data_sub$PATHNO_ENCODED)
          top_nodes <- isolate(input[[paste0('top_nodes_no', i+1)]])
        } else {
          top_nodes <- isolate(input$top_nodes_no)
        }

        if (top_nodes < nrow(data_start_nodes)){
          data_end_nodes_top <- data_end_nodes[1:top_nodes,]
          data_end_nodes_bottom <- data_end_nodes[(top_nodes+1):nrow(data_end_nodes),]  
          size_other <- data_end_nodes_bottom$size %>% sum()
          size_all_other <- data_end_nodes_bottom$size_all %>% sum()
          data_end_nodes <- rbind(data_end_nodes_top, c('Other', size_other,size_all_other)) 
          data_end_nodes$size <- data_end_nodes$size %>% sapply(as.integer)
          data_end_nodes$size_all <- data_end_nodes$size_all %>% sapply(as.double)
          data_end_nodes <- data_end_nodes[order(-data_end_nodes$size),]
        }
        
      }

      ## Remove Missing
      if (isolate(input$remove_missing)){
        data_end_nodes <- data_end_nodes %>%
          filter(NODE_E_ENCODED != 'Missing')
      }
      
      data_end_nodes %>%  
        hchart('column', hcaes(x=NODE_E_ENCODED, y=size, color = size)) %>%
        hc_title(text = "End Nodes",
                 style = list(fontWeight = "bold", fontSize = "30px"),
                 align = "center") %>% 
        hc_yAxis(title = list(text = "Number of Patients"),
                 opposite = FALSE,
                 labels = list(format = "{size}"),
                 max = max_ovr)%>%
        hc_xAxis(title = list(text = NULL),
                 labels = list(rotation = 0,
                               style = list(whiteSpace = "nowrap",
                                            textOverflow = "ellipsis"))) %>%
        hc_tooltip(pointFormat = "Size: {point.y} <br> Percentage: {point.size_all}%",
                   headerFormat = '<span style="font-size: 20px">{point.key}</span><br/>',
                   style = list(fontSize = "20px")) %>%
        hc_plotOptions(column = list(
          dataLabels = list(enabled = TRUE)
        ))
    }
  }
  
  # Rendering Barpl ot of Starting Nodes
  output$BarS <- renderHighchart({
    
    if (input$update == 0){
      return()
    }
    
    data_sub <- data_sub()
    
    high_bar_gen('start', data_sub)
    
  })
  
  
  
  # Rendering Barplot of Ending Nodes
  output$BarE <- renderHighchart({
    if (input$update == 0){
      return()
    }
    
    data_sub <- data_sub()
    
    high_bar_gen('end', data_sub)
  })