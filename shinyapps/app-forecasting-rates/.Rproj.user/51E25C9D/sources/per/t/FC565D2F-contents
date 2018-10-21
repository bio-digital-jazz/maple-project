library(tidyverse)
library(arm)
library(forecast)
library(ggplot2)
library(plotly)

## CONFIG
kColPurple = "#48156BBF"


## DATA PREP
Path = "./Data/"
File = "TimeSeriesGroup.csv"
ADS0 = read_csv(paste0(Path, File))
# first user chooses a cancer
ADS1 = filter(ADS0, CancerGroup=="Respiratory") ### chose from drop down
ADS1 = filter(ADS1, AreaType=="Resident LHD") ### chose from drop down
ADS1 = filter(ADS1, AreaName=="Central Coast LHD") ### chose from drop down
TempTimeSeries = ADS1$CasesAnnual
Future = 10 
ForecastARIMA = forecast(auto.arima(TempTimeSeries), Future)  
MinYear = min(ADS1$DiagnosisYear, na.rm=TRUE) 
YMaxARIMA = round(max(ForecastARIMA$upper) * 1.1  / 5) * 5
ForecastARIMAFinal = round(ForecastARIMA[[4]][Future] / 5) * 5 ### use this number for the auto-authoring - more on this later...
#autoplot(ForecastARIMA, ylim=c(0, YMaxARIMA), ylab="Number of cases")

### End



ui = fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "materialize/css/materialize.min.css")
  ),
  
  
  titlePanel(
    h4("Forecasting cancer incidence in NSW by area")),
  hr(),
  sidebarLayout(
    sidebarPanel(
      p("This visualisation allows users to explore ARMIA forecasts"),

      
      selectInput("cancerGroupFilter", 
                  "Filter by cancer groups", 
                  multiple = FALSE,
                  choices = list(
                                 "Bone and connective tissue", 
                                 "Breast", 
                                 "Cancer unknown primary",
                                 "Colorectal",
                                 "Head and Neck",
                                 "Skin",
                                 "Respiratory"
                                 ), 
                  selected = "All cancer types"),

      selectInput("areaNameFilter", 
                  "Filter by Area name", 
                  multiple = FALSE,
                  choices = list(
                                 "Central Coast LHD",
                                 "Far West LHD",
                                 "Western Sydney LHD",
                                 "Hunter New England LHD",
                                 "Sydney LHD",
                                 "South Western Sydney LHD"
                                 ), 
                  selected = "Sydney LHD"),
      sliderInput("forecastFilter", 
                  "Forecast length", 
                  min = 0, 
                  max = 10, 
                  value = 5)
      
   
      
      
      
    ),
    mainPanel(
      
      
      
      
      tabsetPanel(type = "tabs",
                  tabPanel("Exploration one", 
                           br(),
                           plotlyOutput("forecasts"),
                           br(),
                           
                           htmlOutput("selected_var")
                  ),
                  tabPanel("Exploration two",
                           p('decriptive title')),
                  
                  tabPanel("Data", 
                           br(),
                           p('download button'),
                           downloadButton("downloadData", "Download"),
                           br(),
                           tableOutput("summary")),
                  tabPanel("Notes", 

                           p('asdfasdf')
                           )
      )
      
      
      
      
    )
    
  )
  
)
server = function(input, output) {
  
  output$forecasts = renderPlotly({
    
    print(input$areaNameFilter)
    
   
    ADS1 = filter(ADS0, CancerGroup==input$cancerGroupFilter) ### chose from drop down
    ADS1 = filter(ADS1, AreaType== "Resident LHD") ### chose from drop down
    ADS1 = filter(ADS1, AreaName==input$areaNameFilter) ### chose from drop down
    TempTimeSeries = ADS1$CasesAnnual
    Future = input$forecastFilter
    ForecastARIMA = forecast(auto.arima(TempTimeSeries), Future)  
    MinYear = min(ADS1$DiagnosisYear, na.rm=TRUE) 
    YMaxARIMA = round(max(ForecastARIMA$upper) * 1.1  / 5) * 5
    ForecastARIMAFinal = round(ForecastARIMA[[4]][Future] / 5) * 5
    
    autoplot(ForecastARIMA, ylim=c(0, YMaxARIMA), ylab="Number of cases")
    
    
  })
  
  output$selected_var <- renderText({ 
    

    
    
    paste("<b>Data insights</b><br/>
          The visualisation above shows a forecast of ", input$cancerGroupFilter, ' cancers in the ', input$areaNameFilter, 
          ' area, over a ', input$forecastFilter, ' year period.')
 
  })
  
  
  
}

shinyApp(ui, server)




