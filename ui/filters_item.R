menuItem("Filters", icon = icon("th-list", lib='glyphicon'), tabName = "filters",
               sliderTextInput('perc',
                               label = 'Treatment Percentage', 
                               choices = prettyNum(seq(0,100,0.1), big.mark = ","),
                               selected = '0',
                               grid = FALSE, dragRange = FALSE),

               bsTooltip(id = "perc",
                title = 'Popularity percentage for treatment patterns. Increasing it will remove the smaller paths from the output.'
                ),

               
               hr(),
               tags$div(id='addHere'),
               hr(),
               
               
               materialSwitch("order",
                             label = "Order Nodes",
                             status = 'primary',
                             right = TRUE),


               tags$div(id='path_range_here'),
               
               br()
      )