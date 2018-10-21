library(shiny)
library(dplyr)
library(ggplot2)
library(grid)
library(plotly)

source('./functions.R')



path = "./Data/SurgicalOutcomesLung.Fake.csv"
##path = "../../AnalyticsDatasetsAndReports/AnalyticsDatasets/SurgicalOutcomesLung.Fake.csv"
data = uf.getDataset(path)
meanOfFullDataset = uf.getRawMean(data)


ui = fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "materialize/css/materialize.min.css")
  ),
  
  
  titlePanel(
    h4("Readmission rates for cancer surgery; NSW 2013-16 (Synthetic Data)")),
  hr(),
  sidebarLayout(
    sidebarPanel(
      p("This visualisation allows users to explore post surgical outcomes in NSW facilities"),
      selectInput("genderFilter", 
                  "Filter by sex", 
                  multiple = TRUE,
                  choices = list("All genders" = "All",
                                 "Male" = 1, 
                                 "Female" = 2), 
                  selected = "All"),
      selectInput("cancerTypeFilter", 
                  "Filter by cancer types", 
                  multiple = TRUE,
                  choices = list("All cancer types" = "All",
                                  "Lung" = "lung", 
                                 "Colon" = "colon", 
                                 "Breast" = "breast"), 
                                  selected = "All"),
      
      selectInput("procedureFilter", 
                  "Filter by procedure groups", 
                  multiple = TRUE,
                  choices = list("All procedure groups" = "All",
                                 "Lobectomy" = "Lobectomy", 
                                 "Pneumonectomy" = "Pneumonectomy", 
                                 "Segmental" = "Segmental",
                                 "Wedge" = "Wedge"), 
                  selected = "All"),
      selectInput("lhdChoice", 
                  "Filter by LHD", 
                  multiple = TRUE,
                  choices = list("All Local Health Discricts" = "All",
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
      
      
      radioButtons("radio", 
                   "Mortality rates",
                   choices = list("None" = 1,
                                  "30 day mortality" = 2, 
                                  "90 day mortality" = 3, 
                                  "365 day mortality" = 4), 
                   selected = 1),
      
      
      
      checkboxInput("checkbox", 
                    "Patient required readmission", 
                    value = FALSE)
      
    
      
      
    ),
    mainPanel(
     
    
   
      
      tabsetPanel(type = "tabs",
                  tabPanel("Exploration one", 
                           br(),
                          h4("Readmission within 21 days"),
                           
                            plotlyOutput("surgicalPlot"),
                          br(),
                          
                          textOutput("selected_var")
                          ),
                  tabPanel("Exploration two",
                           p('Heres space for an alternative graphic based on the same data')),
                
                  tabPanel("Data", 
                           br(),
                           downloadButton("downloadData", "Download"),
                           br(),
                           tableOutput("summary")),
                  tabPanel("Notes", 
                           br(),
                           p("Indicators here include Lorem ipsum dolor amet thundercats aesthetic blog vaporware. Glossier art party etsy trust fund. Mlkshk enamel pin humblebrag, fixie letterpress gluten-free echo park pabst la croix hexagon blue bottle jianbing tilde bitters. Vexillologist scenester glossier lyft la croix unicorn williamsburg sartorial etsy lo-fi bitters stumptown art party chicharrones. Hell of kickstarter cronut irony, blue bottle glossier XOXO adaptogen trust fund."),
                      
                           
                           p('asdfasdf'))
      )
      
      
   
      
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
                                  input$lhdChoice,
                                  input$genderFilter,
                                  input$cancerTypeFilter,
                                  input$procedureFilter)
    
    uf.renderVisualization(filteredData, 
                           meanOfFullDataset)
    
    
  })
  
  output$summary <- renderTable({
    
    filteredData = uf.prepareData(data, 
                                  input$ageRange[1], 
                                  input$ageRange[2],
                                  input$lengthOfStay[1],
                                  input$lengthOfStay[2],
                                  input$lhdChoice,
                                  input$genderFilter,
                                  input$cancerTypeFilter,
                                  input$procedureFilter)
    filteredData
    
    
    
  })
  
  output$downloadData <- downloadHandler(
    
    
    
    
    filename = function() {
      return("data.csv")
    },
    content = function(file) {
      write.csv( uf.prepareData(data, 
                                input$ageRange[1], 
                                input$ageRange[2],
                                input$lengthOfStay[1],
                                input$lengthOfStay[2],
                                input$lhdChoice,
                                input$genderFilter,
                                input$cancerTypeFilter,
                                input$procedureFilter), file, row.names = FALSE)
    }
  )
  
  output$selected_var <- renderText({ 
    
    filteredData = uf.prepareData(data, 
                                  input$ageRange[1], 
                                  input$ageRange[2],
                                  input$lengthOfStay[1],
                                  input$lengthOfStay[2],
                                  input$lhdChoice,
                                  input$genderFilter,
                                  input$cancerTypeFilter,
                                  input$procedureFilter)
    
    

    paste0("INSIGHTS: Based on the applying the filters, the highest surgical volume was ", 
          max(filteredData$HospVolume), 
          '\n. The mean readmission rate in this sample was ' , 
          round(mean(filteredData$HospReadmission) * 100, 1),
          '%.\n This sample consists of ages between ', 
          input$ageRange[1], ' and ', input$ageRange[2])
  })
  
  
  
  
 
}

shinyApp(ui, server)




