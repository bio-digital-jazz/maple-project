library(shiny)
library(dplyr)
library(DT)

 
ui <- fluidPage(
  tags$head( 
    tags$link(rel = "stylesheet", type = "text/css", href = "materialize/css/materialize.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css")
  ),   
  fluidRow(             
      
    column(12,          
           br(), 

           tabsetPanel(type = "tabs",        
                       tabPanel("R Resources",        
                                br(),  
                                includeHTML('www/partials/r-resources.html') 
                       ),
                       tabPanel("R Projects",        
                                br(),
                                includeHTML('www/partials/r-projects.html') 
                       ),   
                      tabPanel("Accessing data",   
                                br(),
                                includeHTML('www/partials/accessing-data.html')
                       ), 
                      tabPanel("Governance resources",   
                               br(),
                                 includeHTML('www/partials/governance-resources.html') 
                      )
           )   
    )           
              
                       
      
        
    
  )
  
     
)

server <- function(input, output) {
  
  
  
  
  
  
  
}


shinyApp(ui = ui, server = server)

