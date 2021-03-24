menuItem("Links", icon = icon("th-list", lib='glyphicon'), tabName = "links",
         textInput("link_group",
                   label = "Links",
                   value = "No Colour Grouping Applied"
         ),

         materialSwitch('link_show',
                              label = tags$span(id = "size_label_l", "Show Label sizes"),
                              status = 'primary',
                              right = TRUE),

         
         materialSwitch("mode_switch",
                        label = tags$span(id = "label_link", "Link Mode Switch"), 
                        status = "primary",
                        right = TRUE
         ),

         bsTooltip(id = "link_group",
                title = 'Color group links by regular expression.'
                ),

         bsTooltip(id = "label_link",
                title = 'Color group links by timepoint.'
                ),

         bsTooltip(id = "size_label_l",
                title = 'Show link sizes in the link labels.'
                ),

         

         
         div(id='add_slider_here'),

         materialSwitch("mode_switch2",
                        label = tags$span(id = "label_link2", "Link Mode Switch 2"), 
                        status = "primary",
                        right = TRUE
         ),

         div(id='add_slider_here2'),

         bsTooltip(id = "label_link2",
                title = 'Color group links by NODE_S or NODE_E.'
                ),

         br()
)