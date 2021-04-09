tabItem(tabName = "inputs",
              fluidRow(box(fileInput("data", "Upload Data", accept = c(".csv", ".sas7bdat", '.xlsx')),
                           div(id = 'addRefresh'),
                           DT::dataTableOutput('contents'),
                           br(),
                           width = 12)
              ),
              hr(),
              fluidRow(box(pickerInput('usubjid', 'Select Subject ID', choices = c(), options = list(title = "Nothing Selected", `live-search` = TRUE)),
                           pickerInput('node_s', 'Select Starting Node', choices = c(), options = list(title = "Nothing Selected", `live-search` = TRUE)),
                           pickerInput('node_e', 'Select Ending Node', choices = c(), options = list(title = "Nothing Selected", `live-search` = TRUE)),
                           pickerInput('pathno', 'Select Path Number', choices = c(), options = list(title = "Nothing Selected", `live-search` = TRUE))
              ),
              box(tags$u(h3('Optional Choices', align='left')),
                  pickerInput('pathname', 
                    'Select Path Name', 
                    choices = c(),
                     options = list(title = "Nothing Selected", 
                      `live-search` = TRUE)),
                  pickerInput('filters',
                   'Select Filters to display', 
                   choices = c(),
                   multiple = TRUE, 
                   options = list(`selected-text-format` = "count > 2", 
                    `live-search` = TRUE)))),
              tags$footer(
                hr(),
                p("Copyright 2021 Eli Lilly and Company - Georgios Efstathiadis")
                )
              
      )