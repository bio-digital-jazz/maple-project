#Removing all objects in memory
rm(list=setdiff(ls(), "x"))

#Loading packages
library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(data.table)
library(DT)
library(readr)
library(shinythemes)
library(scales)
library(ggbeeswarm)
library(networkD3)
library(lubridate)

#Loading data
# data <- read.csv('SurgicalActivity.csv')
data <- readRDS(file = "SurgicalOutcomesAppDF.Rda")
data$Count=1

###Building Shiny App
# .nav-tabs {border-bottom: 1px solid rgb(106,66,137);} table.dataTable {line-height:0.7}

##1. Shiny UI ns
#Setting ui as a fluidPage: FluidPage enables Shiny Apps to adjust to screens of varying resolution
ui <- fluidPage(
  #CSS styling
  tags$style(HTML("
                  .tabbable > .nav > li > a {background-color: rgb(255,255,255);  color:black}
                  .nav-tabs>li>a {border: 0px}
                  .nav-tabs>li.active> a {background-color: rgb(106,66,137);  color:white;}
                  .nav-tabs>li.active>a:focus {background-color: rgb(106,66,137);  color:white}
                  .nav-tabs>li.active> a {background-color: rgb(106,66,137);  color:white}
                  .nav-tabs>li.active>a:hover {background-color: rgb(106,66,137);  color:white}
                  .nav-tabs {border-bottom: 1px solid rgb(106,66,137);}
                  .irs-bar {background: rgb(106,66,137); border-top: 1px solid rgb(106,66,137); border-bottom: 1px rgb(106,66,137);}
                  .irs-bar-edge {background: rgb(106,66,137); border: 1px solid rgb(106,66,137); height: 25px; border-radius: 0px; width: 20px;}
                  .irs-to {background: rgb(106,66,137);}
                  .irs-from {background: rgb(106,66,137);}
                  table.dataTable > thead > tr {color: white; background-color:rgb(106,66,137); text-align: center;}
                  table.dataTable > thead > tr > [class='dt-right dt-center sorting'] {text-align: center;}
                  table.dataTable > thead > tr > [class='dt-right dt-center sorting_asc'] {text-align: center;}
                  table.dataTable > thead > tr > [class='dt-right dt-center sorting_desc'] {text-align: center;}
                  table.dataTable {line-height:1}
                  Table {padding-top: 10px;}"
  )
  ),
  #Adding in titlePanel
  titlePanel(div(HTML("<strong><font style = 'color: rgb(106, 66, 137)'>Surgical Mortality</strong></font>"),
                 img(src="CILogo.jpg", align ="right", height=55))),
  #Setting overall layout to be sidebar layout
  sidebarLayout(
    #Inputs to visualisation
    #Sidebar panel
    sidebarPanel(
      #Select input field for Facility
      selectInput(input = "FacilityType",
                  label = "FacilityType:",
                  choices = sort(unique(data$FacilityType))
      ),
      #Select input field for Facility
      selectInput(input = "FacilityName",
                     label = "FacilityName:",
                     choices = split(sort(unique(data$FacilityName)), sort(unique(data$FacilityLHDName)))),
      #Select input field for Cancer
      selectInput(input = "CancerName",
                  label = "CancerName:",
                  choices = sort(unique(data$CancerName))
      ),
      #Select input slider for year
      selectInput(input = "IndicatorName",
                  label = "Indicator:",
                  choices = sort(unique(data$IndicatorName))
      )
    ),
    #Main panel
    mainPanel(
      #Creating tabs for main panel
      tabsetPanel(type = "tabs",
                  #Tab 1: Activity
                  tabPanel("Activity",
                           plotOutput("SurgicalOutcomesGG"),
                  dataTableOutput("test"),
                  dataTableOutput("test1"),
                  textOutput("test2"),
                  dataTableOutput("test3"))
      )
    )
    
  )
  )

##2. Shiny server
#Setting up Shiny server
server <- function(input, output, session) {
  
  #Setting observeEvent to update Cancer select input as user selects LHD and Facility

  observeEvent({input$FacilityType, {
    updateSelectInput(session, "FacilityName", choices = ddf1$FacilityName)
  })

  # #Setting observeEvent to update Cancer select input as user selects LHD and Facility
  # observeEvent(c(input$FacilityType, input$FacilityName), {
  #   updateSelectInput(session, "CancerName",
  #                     choices = sort(unique(data$CancerName[data$FacilityType == input$FacilityType &
  #                                                               data$FacilityName == input$FacilityName])))
  # })
  # 
  # #Setting observeEvent to update Cancer select input as user selects LHD and Facility
  # observeEvent(c(input$FacilityType, input$FacilityName, input$CancerName), {
  #   updateSelectInput(session, "IndicatorName",
  #                     choices = sort(unique(data$IndicatorName[data$FacilityType == input$FacilityType &
  #                                                             data$FacilityName == input$FacilityName &
  #                                                               data$CancerName == input$CancerName])))
  # })

  
  # Generating plots
  # TAB 1: Activity
  # helper function
  fMakeFunnelPolygons = function(Mean, XMax, YMin=0, YMax=1) {

    #
    if(Mean > 1) Mean = 1
    if(Mean < 0) Mean = 0

    # Conpute the funnels based on beta distribution
    XMax = XMax * 1.0
    P = round(Mean, 2)
    Q = 1 - P
    N = 1:XMax
    E = N * P

    Y2SDLow  = qbeta(0.023, E, N-E+1)
    Y2SDHigh = qbeta(0.977, E+1, N-E)

    Y3SDLow  = qbeta(0.0013, E, N-E+1)
    Y3SDHigh = qbeta(0.9987, E+1, N-E)

    # clip the polygons here based on the 2x YLims
    Y2SDLow[Y2SDLow < YMin] = YMin
    Y2SDHigh[Y2SDHigh > YMax] = YMax
    Y2SD = c(Y2SDHigh, rev(Y2SDLow))

    Y3SDLow[Y3SDLow < YMin] = YMin
    Y3SDHigh[Y3SDHigh > YMax] = YMax
    Y3SD = c(Y3SDHigh, rev(Y3SDLow))

    X = c(N, rev(N))

    data.frame(X=X, Y2SD = Y2SD, Y3SD = Y3SD)
  }


  #Generating react dataframe for DFTitanic
  rdfTab1Data <- reactive({
    subset(data, FacilityType == input$FacilityType &
             FacilityName %in% input$FacilityName &
             CancerName == input$CancerName &
             IndicatorName == input$IndicatorName
    )
  })


  # get NSW mean based on user selection
  rdfTab1NSW = reactive({
    subset(data, FacilityLHDName == "NSW" &
             FacilityType == input$FacilityType &
             FacilityName == "NSW" &
             CancerName == input$CancerName &
             IndicatorName == input$IndicatorName
    )
  })

  NSWMean = reactive({rdfTab1NSW()$FacilityMeanRaw})


  # get info on other facilities in state
  rdfTab1FacilityOther = reactive({
    subset(data,
           FacilityType == input$FacilityType &
             FacilityName != "NSW" &
             CancerName == input$CancerName &
             IndicatorName == input$IndicatorName
    )
  })


  output$test<-renderDataTable(rdfTab1Data())
  output$test1<-renderDataTable(rdfTab1NSW())
  output$test2<-renderText(NSWMean())
  output$test3<-renderDataTable(rdfTab1FacilityOther())

  # derive maximum x-axis value
  XMax = reactive({max(100, rdfTab1FacilityOther()$FacilityN * 1.1, na.rm=TRUE)})


  # Plot the result(s)
  output$SurgicalOutcomesGG = renderPlot({ggplot(data = rdfTab1Data()) +
    coord_cartesian(xlim=c(0,XMax()), ylim=c(0,1)) +
    geom_polygon(data = fMakeFunnelPolygons(NSWMean(), XMax(), 0, 1), aes(x = X, y = Y2SD), fill = "#BEBEBE70", colour = NA) +
    geom_polygon(data = fMakeFunnelPolygons(NSWMean(), XMax(), 0, 1), aes(x = X, y = Y3SD), fill = "#BEBEBE70", colour = NA) +
    geom_hline(yintercept = NSWMean(), colour="dodgerblue2") +
    geom_point(aes(x=FacilityN, y=FacilityMeanMarg), size=2) +
    geom_rug(data=rdfTab1FacilityOther(), aes(FacilityN), colour="darkgrey") +
    labs (x = "Total volume 2014-17", y="Proportion")})
  
}

#Run App
shinyApp(ui, server)

