menuItem("Graph Styles", icon = icon("th-list", lib='glyphicon'), tabName = "graph styles",
    
    h5('Links'),
    materialSwitch("link_static_opacity",
        label = "Static Opacity",
        status = 'primary',
        right = TRUE),
    div(id = 'link_static_opacity_here'),
    hr(),

    h5('Nodes'),
    materialSwitch("node_static_opacity",
        label = "Static Opacity",
        status = 'primary',
        right = TRUE),

    div(id = 'node_static_opacity_here'),

    sliderTextInput('node_padding',
                  label = 'Padding', 
                  choices = prettyNum(0:25, big.mark = ","),
                  selected = '10',
                  grid = FALSE, dragRange = FALSE),

    sliderTextInput('node_width',
                  label = 'Width', 
                  choices = prettyNum(1:30, big.mark = ","),
                  selected = '15',
                  grid = FALSE, dragRange = FALSE),

    sliderTextInput('node_font_size',
                  label = 'Text Size', 
                  choices = prettyNum(1:30, big.mark = ","),
                  selected = '15',
                  grid = FALSE, dragRange = FALSE),

    textInput("node_font",
               label = "Text Font",
               value = ""
    ),

    textInput("node_units",
               label = "Text Units",
               value = ""
    ),


    hr(),

    h5('Legend'),
    div(id = 'legend_style_here'),
    hr(),

    h5('Timepoints'),
    div(id = 'timepoint_style_here'),

    br()
)