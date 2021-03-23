menuItem("Nodes", icon = icon("th-list", lib='glyphicon'), tabName = "nodes",
               textInput("color",
                         label = "Colour Grouping",
                         value = "No Colour Grouping Applied"
               ),

               bsTooltip(id = "color",
                title = 'Color group nodes by regular expression.'
                ),
               
               pickerInput("color_1",
                           label = "Colour for Group 1",
                           choices = c()),
               
               pickerInput("color_2",
                           label = "Colour for Group 2",
                           choices = c()
               ),
               
               pickerInput("color_3",
                         label = "Colour for Discontinued",
                         choices = c()
               ),
               
               pickerInput("color_4",
                         label = "Colour for No Treatment",
                         choices = c()
               ),

               materialSwitch('remove_labels',
                              label = tags$span(id = "remove_labels_label", "Hide Node Labels"),
                              status = 'primary',
                              right = TRUE),

               materialSwitch('node_unique',
                              label = tags$span(id = "node_unique_label", "Colour Group Nodes"),
                              status = 'primary',
                              right = TRUE),

               div(id ='node_unique_after'),

               bsTooltip(id = "node_unique_label",
                title = 'Colour each unique node differently.'
                ),
               
               materialSwitch('node_show',
                              label = tags$span(id = "size_label", "Show Node sizes"),
                              status = 'primary',
                              right = TRUE),

               bsTooltip(id = "size_label",
                title = 'Show node sizes in the node labels.'
                ),
               
               materialSwitch('top_nodes',
                              label = tags$span(id = "top_label", 'Show Largest Nodes'),
                              status = 'primary',
                              right = TRUE),

               bsTooltip(id = "top_label",
                title = 'Only show the largest nodes and group all others to a single node named "Other".'
                ),

               checkboxInput('advanced_top',
                              label = tags$span(id = "advanced_label", 'Advanced Options')),

               bsTooltip(id = "advanced_label",
                title = 'Configure the top nodes of each timepoint separately.'
                ),

               
               div(id = 'add_top_nodes_slider'),
               
               br()
      )