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
                       tabPanel("Project overview",  
                                br(),   
                                includeHTML('www/partials/overview.html') 
                       ),
                       tabPanel("Architecture", 
                                br(),
                                includeHTML('www/partials/architecture.html')
                                
                       ) 
           )    
    )  
    
    
    
    
    
  )
  
  
)

server <- function(input, output) {
  
  
  
  
  
  
  
}


shinyApp(ui = ui, server = server)

