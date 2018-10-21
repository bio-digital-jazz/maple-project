library(shiny)
library(dplyr)
library(ggplot2)
library(grid)
library(plotly)

source('./functions.R')


path = "../../../SharedData/SurgicalOutcomesLung.Fake.csv"
data = uf.getDataset(path)
meanOfFullDataset = uf.getRawMean(data)


ui = fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "materialize/css/materialize.min.css")
  ),
  
  
  titlePanel("Exploring cancer surgical rates"),
  hr(),
  sidebarLayout(
    sidebarPanel(
      p("This visualisation allows users to explore surgical volumes in NSW"),
      selectInput("genderFilter", 
                  "Filter by gender", 
                  multiple = TRUE,
                  choices = list("All genders" = 1,
                                 "Lung" = 2, 
                                 "Colon" = 3, 
                                 "Breast" = 4), 
                  selected = 1),
      selectInput("select", 
                  "Filter by cancer types", 
                  multiple = TRUE,
                  choices = list("All cancer types" = 1,
                                  "Lung" = 2, 
                                 "Colon" = 3, 
                                 "Breast" = 4), 
                                  selected = 1),
      
      selectInput("procedureGroup", 
                  "Filter by procedural groups", 
                  multiple = TRUE,
                  choices = list("All procedure groups" = 1,
                                 "Lobectomy" = 2, 
                                 "Wedge" = 3, 
                                 "Segmental" = 4), 
                  selected = 1),
      selectInput("lhdChoice", 
                  "Filter by LHD", 
                  multiple = TRUE,
                  choices = list("All LHDs" = "All",
                                 "X770" = "X770",
                                 "X710" = "X710",
                                 "X750" = "X750"), 
                  selected = "All"),
      sliderInput("ageRange", 
                  "Age range", 
                  min = 0, 
                  max = 100, 
                  value = c(20, 80)),
      sliderInput("lengthOfStay", 
                  "Length of stay", 
                  min = 1, 
                  max = 50, 
                  value = c(1, 50)),
      checkboxInput("checkbox", 
                    label = "30 day mortality", 
                    value = FALSE),
      checkboxInput("checkbox", 
                    label = "90 day mortality", 
                    value = FALSE),
      checkboxInput("checkbox", 
                    label = "365 day mortality", 
                    value = FALSE),
      checkboxInput("checkbox", 
                    label = "Readmitted", 
                    value = FALSE)
      
    
      
      
    ),
    mainPanel(
     
      plotlyOutput("surgicalPlot")
   
      
    )
    
  )
  
)
server = function(input, output) {
  
  
  output$surgicalPlot = renderPlotly({
    
    
    filteredData = uf.prepareData(data, 
                                  input$ageRange[1], 
                                  input$ageRange[2],
                                  input$lengthOfStay[1],
                                  input$lengthOfStay[2],
                                  input$lhdChoice)
    uf.renderVisualization(filteredData, 
                           meanOfFullDataset)
    
    
  })
  
  
 
}

shinyApp(ui, server)




