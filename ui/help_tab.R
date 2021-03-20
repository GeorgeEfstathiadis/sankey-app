tabItem(tabName = "help",
              box(markdown("**App deployed here**: [![app](https://img.shields.io/badge/app-Heroku-yellow?style=flat&labelColor=gray)](https://sankey-treatment-patterns.herokuapp.com/)  
                            **Code can be found here**: [![repo](https://img.shields.io/badge/repo-GitHub-success?style=flat&labelColor=gray)](https://github.com/GeorgeEfstathiadis/sankey-app)"),
                width = 12),
              box(h2('Input Data (csv, sas7bdat or xlsx file)'),
                  br(),
                  h3('Required Columns'),
                  br(),
                  HTML('<p>
                          <b>USUBJID</b>: Unique subject id for each person.<br>
                         <b>NODE_S</b>: Name of the starting node for the path.<br>
                         <b>NODE_E</b>: Name of the ending node for the path.<br>
                         <b>PATHNO</b>:  Number of the path (starting from 1).<br>
                         Each row is a unique combination of the USUBJID and PATHNO, meaning that it contains information for the link containing a person at a specific path.
                         Thus, the data should include for each unique id where did it start and where it ended at each timepoint (path no.).
                         </p>'),
                  br(),
                  h3('Optional Columns'),
                  HTML('<p>
                          <b>PATHNAME</b>:  An encoding for the names of each path (e.g. for PATHNO 1, PATHNAME is DAY1 - DAY2). Preferable format is of "{start_timepoint} - {end_timepoint}" for aditional functionality. Make sure PATHNAME is correctly mapped to PATHNO.
<br>
                         <b>FILTERS</b>: Any kind of filters.<br>
                         </p>'),
                  br(),
                  width = 12),
              fluidRow(box(h4('Final Dataset Format'),
                           img(src='data_example.png', height="50%", width="70%")),
                       box(h4('Sankey Output'),
                           img(src='output_example.png',height="50%", width="70%"))),
              fluidRow(box(h2('Information on transforming your data in our format'),
                br(),
                HTML('<p>
                         If data in wide format, meaning one row per subject and a column per timepoint you may use the following template to convert your data in R and prepare them for use.
                         </p>'),
                downloadButton("download_wide",
                              label = "Template"),
                textOutput("keep_alive"),
                width = 12))
              
              
      )
