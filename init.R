my_packages <- c("shiny", "shinydashboard", "shinycssloaders", "shinyBS",
	"shinyWidgets", "networkD3", "highcharter", "DT", "htmltools", "htmlwidgets",
	"dplyr", "stringr", "readxl", "haven", "RColorBrewer", "webshot")

install_if_missing <- function(p){
	if (p %in% rownames(installed.packages()) == FALSE){
		install.packages(p)
	}
}

invisible(sapply(my_packages, install_if_missing))
webshot::install_phantomjs()
