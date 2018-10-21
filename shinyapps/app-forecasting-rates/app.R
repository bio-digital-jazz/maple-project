library(tidyverse)
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



ui = fluidPage(title = "Forecasting cancer incidence in NSW by area",
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "materialize/css/materialize.min.css")),
    tags$style(HTML(".tabbable > .nav > li > a {background-color: rgb(255,255,255);  color:black}
                    .nav-tabs>li>a {border: 0px}
                    .nav-tabs>li.active> a {background-color: rgb(106,66,137);  color:white;}
                    .nav-tabs>li.active>a:focus {background-color: rgb(106,66,137);  color:white}
                    .nav-tabs>li.active> a {background-color: rgb(106,66,137);  color:white}
                    .nav-tabs>li.active>a:hover {background-color: rgb(106,66,137);  color:white}
                    .nav-tabs {border-bottom: 1px solid rgb(106,66,137);}
                    .irs-bar {background: rgb(106,66,137); border-top: 1px solid rgb(106,66,137); border-bottom: 1px rgb(106,66,137);}
                    .irs-bar-edge {background: rgb(106,66,137); border: 1px solid rgb(106,66,137); border-radius: 0px; width: 20px;}
                    .irs-to {background: rgb(106,66,137);}
                    .irs-from {background: rgb(106,66,137);}
                    .irs-single {background: rgb(106,66,137);}
                    .glyphicon {color: rgb(106,66,137);}
                    table.dataTable > thead > tr {color: white; background-color:rgb(106,66,137); text-align: center;}
                    table.dataTable > thead > tr > [class='dt-right dt-center sorting'] {text-align: center;}
                    table.dataTable > thead > tr > [class='dt-right dt-center sorting_asc'] {text-align: center;}
                    table.dataTable > thead > tr > [class='dt-right dt-center sorting_desc'] {text-align: center;}
                    table.dataTable {line-height:1}
                    Table {padding-top: 10px;}"
    )
    ),
  
  titlePanel(div(HTML("<strong><font style = 'color: rgb(106, 66, 137)'>Forecasting cancer incidence in NSW by area</strong></font>"),
                 img(src="CILogo.jpg", align ="right", height=55))),
  sidebarLayout(
    sidebarPanel(
      p("Single Source of Truth!"),
      selectInput("cancerGroupFilter", 
                  "Filter by cancer clinical group", 
                  multiple = FALSE,
                  choices = list(
                                 "Breast", 
                                 "Colorectal",
                                 "Head and Neck",
                                 "Skin",
                                 "Respiratory"
                                 ), 
                  selected = "Colorectal"),

      selectInput("areaNameFilter", 
                  "Filter by LHD", 
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
                  min = 1, 
                  max = 10, 
                  value = 5,
                  animate =
                    animationOptions(interval = 500, loop = TRUE))

    ),
    mainPanel(
      
      
      
      
      tabsetPanel(type = "tabs",
                  tabPanel("Timeseries forecast", 
                           br(),
                           plotlyOutput("forecasts"),
                           br(),
                           
                           htmlOutput("selected_var")
                  ),
                  tabPanel("Data", 
                           br(),
                           downloadButton("downloadData", "Download"),
                           br(),
                           tableOutput("summary")),
                  tabPanel("Notes", 

                           p('Time series forecast are from an ARIMA model with drift etc'),
                           br(),
                           p('Notes on the data source'),
                           br(),
                           p('Notes on the timeseries forecasting methodology - Techy'),
                           p('Notes on the timeseries forecasting methodology - Plain English'),
                           br(),
                           p('Notes on how to reference these data'),
                           br(),
                           p('etc etc Lorem ipsum dolor amet mlkshk ugh sartorial shabby chic schlitz XOXO portland fam ethical sriracha swag af. 
                             Plaid try-hard forage lomo sustainable. Fam fixie distillery succulents +1 crucifix tacos, keffiyeh aesthetic cardigan. 
                             Activated charcoal godard air plant, helvetica chambray bicycle rights YOLO pickled squid. Marfa affogato twee humblebrag.')
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
#    TempTimeSeries = ADS1$CasesAnnual
    TempTimeSeries = ts(ADS1$CasesAnnual, 1972)
    Future = input$forecastFilter
    ForecastARIMA = forecast(auto.arima(TempTimeSeries), Future)  
    MinYear = min(ADS1$DiagnosisYear, na.rm=TRUE) 
    YMaxARIMA = round(max(ForecastARIMA$upper) * 1.1  / 5) * 5
    ForecastARIMAFinal = round(ForecastARIMA[[4]][Future] / 5) * 5
    
    autoplot(ForecastARIMA, 
#             main=paste0("Forecast for ", input$areaNameFilter, ": ", input$cancerGroupFilter),
             main="",
             xlim=c(1972,2025), ylim=c(0, YMaxARIMA), 
             xlab="Year",
             ylab="Number of cases")

  })
  
  output$selected_var <- renderText({ 
    
    paste("<b>Observations</b><br/>
          The visualisation above shows actual cases of ", input$cancerGroupFilter, ' cancer in the ', input$areaNameFilter, 
          ' area over the period 1972 - 2014 and forecast cases 2015 - ', 2014 + input$forecastFilter, '.
          <br/> <br/>
          <b>Insights</b><br/> Forecasted values here...')
  })
}

shinyApp(ui, server)




