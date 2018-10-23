dataElementsTab <-     tabPanel("Data elements",  
                         fluidRow(  
                           column(12,  
                                  
                                  br()    
                           ) # end col   
                         ), #end row
                         
                   
                         
                         
                         fluidRow(
                           
                           column(12,
                                  br(),
                                  h4("Data elements"),
                                  p('There are currently 195 unique data elements listed in the Cancer Institute Data Dictionary and the attributes of these elements 
                                    can be found in the table below. Note that where a data element has a true status (1) for "name is conformed", this indicates 
                                    that the data element uses the internal Cancer Institute naming conventions. As such, its meaning will hold across 
                                    different data sets. For example the "APostcode" has the same meaning in the NSWCR Analytics datasets and the Breastscreen dataset.
                                    Note that is not appropriate to conformed names in some data assets (for example, the Public Health Register) and, as such, 
                                    care must be taken when undertaking analytics with these data assets'),
                                  DT::dataTableOutput("dataElements")
                           )
                           
                           
                         ) # end row
)# end tabPanel