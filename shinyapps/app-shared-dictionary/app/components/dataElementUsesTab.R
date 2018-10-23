dataElementUsesTab <-     tabPanel("Data element uses",  
                                fluidRow(  
                                  column(12,  
                                         
                                         br()    
                                  ) # end col   
                                ), #end row
                                fluidRow(   
                                  column(12,
                                         h4('Global filters'),
                                         p('A number of different global filters can be applied to all data elements and data linkages.')
                                         
                                  )
                                ), # end row
                                fluidRow(
                                  
                                  column(3,
                                         selectInput("cancerTypeFilter", 
                                                     "Filter by data source", 
                                                     multiple = TRUE,
                                                     choices = list(
                                                       "NSWCR Software Application Data Extract" = 1, 
                                                       "OCSIP Extract" = 2, 
                                                       "Medical Oncology Information System Data Extract" = 3,
                                                       "Electronic Radiation Oncology Data Extract" = 4
                                                       
                                                     ), 
                                                     selected = "None")
                                  ),
                                  column(3,
                                         selectInput("cancerTypeFilterOne", 
                                                     "Filter by analytics dataset", 
                                                     multiple = TRUE,
                                                     choices = list("NSWCR Dataset" = 1,
                                                                    "NSWCR Outpatient Radiation Oncology Dataset" = 2, 
                                                                    "NSWCR Outpatient Systemic Therapies Onclogy Dataset" = 3, 
                                                                    "NSWCR Incidence and Mortality Dataset" = 4), 
                                                     selected = "None")
                                  ),
                                  column(3,
                                         selectInput("cancerTypeFilterOne", 
                                                     "Filter by category", 
                                                     multiple = TRUE,
                                                     choices = list("Person" = 1,
                                                                    "Case" = "lung",  
                                                                    "Preventative Episode" = 3, 
                                                                    "Screening Episode" = 4,
                                                                    "Treatment Epsisode" = 5,
                                                                    "Organisation" = 6,
                                                                    "Location",
                                                                    "Health Professional" = 7
                                                     ), 
                                                     selected = "None")
                                  ),
                                  column(3,
                                         selectInput("cancerTypeFilterOne", 
                                                     "Filter by data type", 
                                                     multiple = TRUE,
                                                     choices = list("Boolean" = 1,
                                                                    "Char" = 2,
                                                                    "Date" = 3,
                                                                    "Float" = 4,
                                                                    "Integer" = 5,
                                                                    "VarChar (variable length)" = 6), 
                                                     selected = "None")
                                  )
                                  
                                  
                                  
                                ), # end row
                                
                                
                                fluidRow(
                                  
                                  column(12,
                                         br(),
                                         h4("Data elements"),
                                         p('A list of unique data elements in the Cancer Institute can be seen in the table below.'),
                                         br(),
                                         DT::dataTableOutput("dataElementsTable")
                                  )
                                  
                                  
                                ), # end row
                                fluidRow(
                                  column(12,
                                         br(),
                                         h4("Transformation rules"),
                                         p("A list of data linkages (tracking the way that different data elements are sourced and transformed for use in analytics datasets) can be seen in the table below."),
                                         br(),
                                         DT::dataTableOutput("rulesAndCoverage")
                                  )
                                  
                                ) # end row
)# end tabPanel