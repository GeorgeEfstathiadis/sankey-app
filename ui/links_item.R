menuItem("Links", icon = icon("th-list", lib='glyphicon'), tabName = "links",
         textInput("link_group",
                   label = "Links",
                   value = "No Colour Grouping Applied"
         ),
         
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

         

         
         div(id='add_slider_here'),
         br()
)