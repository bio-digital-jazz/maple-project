coverageQualityUsageTab <- tabPanel("Coverage and quality metrics", 
                                    fluidRow(
                                      column(12,
                                             br(),
                                             h4("Quality and coverage"),
                                             br()
                                      )
                                      
                                      
                                    ), # end row
                                    fluidRow(
                                      column(4,
                                             selectInput("dataElementChoice", 
                                                         "Choose data elements to visualise coverage", 
                                                         multiple = TRUE,
                                                         choices = list("DE425 - ABSDeathCauseCode" = 1,
                                                                        "DE517 - ABSDeathCauseCode" = 2, 
                                                                        "DE518 - AccommTypeId" = 3, 
                                                                        "DE404 - ActualDose" = 4,
                                                                        "DE426 - ActualFractions" = 5), 
                                                         selected = "None")
                                      )
                                      
                                      
                                      
                                    ), # end row
                                    fluidRow(
                                      column(12,
                                             plotlyOutput("coveragePlot"),
                                             hr())
                                      
                                      
                                      
                                    ), # end row
                                    fluidRow(
                                      column(4,
                                             selectInput("dataElementChoice", 
                                                         "Choose data elements to explore usage notes", 
                                                         multiple = TRUE,
                                                         choices = list("DE425 - ABSDeathCauseCode" = 1,
                                                                        "DE517 - ABSDeathCauseCode" = 2, 
                                                                        "DE518 - AccommTypeId" = 3, 
                                                                        "DE404 - ActualDose" = 4,
                                                                        "DE426 - ActualFractions" = 5), 
                                                         selected = "None")
                                      )
                                      
                                      
                                      
                                    ) # end row
                                    
)