my_packages <- c("shiny", "shinydashboard", "shinycssloaders", "shinyBS",
	"shinyWidgets", "networkD3", "highcharter", "DT", "htmltools", "htmlwidgets",
	"dplyr", "stringr", "readxl", "haven", "RColorBrewer")

for (p in my_packages){
  if (p %in% rownames(installed.packages()) == FALSE){
    install.packages(p)
  }
}
