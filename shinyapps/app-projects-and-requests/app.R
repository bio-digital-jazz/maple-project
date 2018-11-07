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
    
    projects <- data.frame(fromJSON("http://localhost:8000/api/projects/all-projects/"));
    
    print(projects)
    
  
     lapply(1:nrow(projects), FUN = function(i) {
        renderUI({
         tagList(
           fluidRow(
             hr(style="height:1px;border:none;color:#333;background-color:#333;"),
             column(2,
                    tags$b("Date recieved:")
                    ),
             column(2,
                    div(projects[i,"date_received"])),
             column(2,
                    tags$b("Requested by")),
             column(6,
                    div(projects[i,"requestor"]))
             ),
            fluidRow(
              column(12,
                     tags$b("Summary:")),
              column(12,
                     div(projects[i,"title_or_summary"]))
              
            ),
           fluidRow(
             column(12,
                    tags$b("Detail:")),
             column(12,
                    div(projects[i,"detail"]),
                    br()),
             column(12,
                    tags$div(
                      div(class="left-align",
                          a(class="waves-effect waves-light btn", "edit", 
                            HTML("<i class='material-icons left'>edit</i>"), 
                            href=paste0("http://localhost:8000/admin/projects/project/",projects[i,"id"]))
                          )))
             
           )
           
           )
       })
     })
   
  }
    
   
   
  )
  

 
  
 
}

shinyApp(ui = ui, server = server)

