source('init.R')

library(shiny, warn.conflicts = FALSE)
library(shinydashboard, warn.conflicts = FALSE)
library(shinyWidgets, warn.conflicts = FALSE)
library(shinycssloaders, warn.conflicts = FALSE)
library(shinyBS, warn.conflicts = FALSE)

library(networkD3, warn.conflicts = FALSE)
library(highcharter, warn.conflicts = FALSE)
library(DT, warn.conflicts = FALSE)

library(htmltools, warn.conflicts = FALSE)
library(htmlwidgets, warn.conflicts = FALSE)


ui <- dashboardPage(
  dashboardHeader(title = 'Sankey App'),
  dashboardSidebar(
    sidebarMenu( 
      id = 'tabs',
      menuItem('Inputs', tabName = 'inputs', icon =  icon("pencil", lib = "glyphicon")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      source(file.path("ui", "filters_item.R"),  local = TRUE)$value,
      source(file.path("ui", "nodes_item.R"),  local = TRUE)$value,
      source(file.path("ui", "links_item.R"),  local = TRUE)$value,
      source(file.path("ui", "general_styles_item.R"),  local = TRUE)$value,
      source(file.path("ui", "styles_item.R"),  local = TRUE)$value,
      source(file.path("ui", "download_item.R"),  local = TRUE)$value, 
      menuItem("Help", icon = icon("question-sign", lib='glyphicon'), tabName = "help"),
      actionBttn("update", label = "Update Graph", style = "pill", color = "success")
     
      
    )
  ),
  dashboardBody(
    ## Styles and scripts

    htmlOutput('d3', container = tags$script, src = 'https://d3js.org/d3.v4.min.js'),
    htmlOutput('d3tip', container = tags$script, src = 'https://cdnjs.cloudflare.com/ajax/libs/d3-tip/0.7.1/d3-tip.min.js'),
    htmlOutput('d3transition', container = tags$script, src = 'https://d3js.org/d3-transition.v2.min.js'),

    tags$script(src = "JS/keepalive.js"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "CSS/style.css")
      ),

    ## Pages
    tabItems(
      source(file.path("ui", "inputs_tab.R"),  local = TRUE)$value,      
      source(file.path("ui", "dashboard_tab.R"),  local = TRUE)$value,
      source(file.path("ui", "help_tab.R"),  local = TRUE)$value
    )
  ),
  skin = 'black'
)