
menuItem("Download", icon = icon("download-alt", lib = "glyphicon"), tabName = "download",
         br(),
         fluidRow(column(1),
                  column(5,downloadButton("download",
                        label = "HTML")),
                  column(4,downloadButton("downloadsvg",
                        label = "SVG")),
                  column(2)
                  ),
         br()
)