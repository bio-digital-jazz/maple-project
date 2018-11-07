library(shiny)
library(ggplot2)
library(dplyr)
library(jsonlite)

bcl <- read.csv("./data/bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/css/materialize.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/icon?family=Material+Icons")
    
  ),
  fluidRow(
    column(4, 
           div("")),
    column(4, 
           textInput("text", label = "", placeholder = "Search")
           ),
    column(4,
           div(''))
  ),
  
  fluidRow(
    column(12,
           htmlOutput('test'),
           br(),
           br(),
           tableOutput("results"))
    
  
  )

)

server <- function(input, output, session) {
  
  



  output$test <- renderUI({
    
    apps_notebooks <- data.frame(fromJSON("http://localhost:8000/api/apps_or_notebooks/all-apps_or_notebooks/"));
    
  
  
     lapply(1:nrow(apps_notebooks), FUN = function(i) {
        renderUI({
         tagList(
           fluidRow(
             hr(style="height:1px;border:none;color:#333;background-color:#333;"),
             
             column(12,
                   tags$b("Title:"),
                   div(apps_notebooks[i,"title"]),
                   tags$b("Summary:"),
                   div(apps_notebooks[i,"description"])
                   ),
            
       
    
             column(12,
                    br(),
                    tags$b("Tags:"),
                    tags$div(
                      div(class="left-align",
                          HTML('<br/><span class="chip #7b1fa2 orange darken-2 white-text">Incidence<i class="close material-icons">close</i></span>
                               <span class="chip #7b1fa2 orange darken-2 white-text">Incidence<i class="close material-icons">close</i></span>
                               <span class="chip #7b1fa2 orange darken-2 white-text">Incidence<i class="close material-icons">close</i></span>'))
                          
                          ))
             
           )
           
           )
       })
     })
   
  }
    
   
   
  )
  
 
 
  
 
}

shinyApp(ui = ui, server = server)

