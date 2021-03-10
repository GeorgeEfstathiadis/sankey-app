tabItem(tabName = "help",
              box(h2('Input Data (csv, sas7bdat or xlsx file)', style = 'color:steelblue;text-decoration: underline;text-decoration-color: steelblue;'),
                  br(),
                  h3('Required Columns', style = 'color:steelblue;text-decoration: underline;text-decoration-color: steelblue;'),
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
                  h3('Optional Columns', style = 'color:steelblue;text-decoration: underline;text-decoration-color: steelblue;'),
                  HTML('<p>
                          <b>PATHNAME</b>:  An encoding for the names of each path (e.g. for PATHNO 1, PATHNAME is DAY1). Make sure PATHNAME is correctly mapped to PATHNO.<br>
                         <b>FILTERS</b>: Any kind of filters.<br>
                         </p>'),
                  br(),
                  width = 12),
              fluidRow(box(h4('Final Dataset Format', style = 'color:steelblue;text-decoration: underline;text-decoration-color: steelblue;'),
                           img(src='data_example.png', height="50%", width="70%")),
                       box(h4('Sankey Output', style = 'color:steelblue;text-decoration: underline;text-decoration-color: steelblue;'),
                           img(src='output_example.png',height="50%", width="70%"))),
              fluidRow(box(h2('Information on transforming your data in our format', style = 'color:steelblue;text-decoration: underline;text-decoration-color: steelblue;'),
                br(),
                HTML('<p>
                         If data in wide format, meaning one row per subject and a column per timepoint you may use the following template to convert your data in R and prepare them for use.
                         </p>'),
                downloadButton("download_wide",
                              label = "Template"),
                width = 12))
              
              
      )