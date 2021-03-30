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

               tags$div(id='order_option_here'),


               tags$div(id='path_range_here'),

               checkboxInput('timepoint_labels',
                              label = tags$span(id = "timepoint_label", 'Move Timepoints on graph')),

               bsTooltip(id = "timepoint_label",
                title = 'If the timepoints extracted from the PATHNAME are of correct format they will be placed on top of the graph.'
                ),

               checkboxInput('remove_missing',
                              label = tags$span(id = "remove_missing_label", 'Remove Missing')),

               bsTooltip(id = "remove_missing_label",
                title = 'Remove all missing Nodes and Links from the graph.'
                ),

               checkboxInput('manual_colors',
                              label = tags$span(id = "manual_colors_label", 'Prompt Manual Color Editing')),

               bsTooltip(id = "manual_colors_label",
                title = 'Click on any node or link to change its color.'
                ),
               
               br()
      )