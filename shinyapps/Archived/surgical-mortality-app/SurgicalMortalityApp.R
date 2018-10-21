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

options(shiny.usecairo=T)

#Loading data
# data <- read.csv('SurgicalActivity.csv')
data <- readRDS(file = "SurgicalOutcomesAppDF.Rda")
data1 <- readRDS(file = "SurgicalOutcomesAppDF2.Rda")
data$Count=1

###Building Shiny App
# .nav-tabs {border-bottom: 1px solid rgb(106,66,137);} table.dataTable {line-height:0.7}

##1. Shiny UI ns
#Setting ui as a fluidPage: FluidPage enables Shiny Apps to adjust to screens of varying resolution
ui <- fluidPage(title="test",
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
      #Select input field for LHD
      selectInput(input = "FacilityLHDName",
                  label = "Local health district:",
                  choices = sort(unique(data$FacilityLHDName))
      ),
      #Select input field for Facility
      selectInput(input = "FacilityType",
                  label = "FacilityType:",
                  choices = rev(sort(unique(data$FacilityType)))
      ),
      #Select input field for Facility
      selectInput(input = "FacilityName",
                  label = "FacilityName:",
                  choices = sort(unique(data$FacilityName))
      ),
      #Select input field for Cancer
      selectInput(input = "CancerName",
                  label = "CancerName:",
                  choices = sort(unique(data$CancerName))
      ),
      #Select input slider for year
      selectInput(input = "IndicatorName",
                  label = "Indicator:",
                  choices = sort(unique(data$IndicatorName))
      ),
      #Checkbox for AllOtherOutsideLHD
      checkboxInput("AllOtherOutsideLHD", label = "All other outside LHD", value = FALSE),
      #Checkbox for AllOtherWithinLHD
      checkboxInput("AllOtherWithinLHD", label = "All other within LHD", value = FALSE)
    ),
    #Main panel
    mainPanel(
      #Creating tabs for main panel
      tabsetPanel(type = "tabs",
                  #Tab 1: Activity
                  tabPanel("Funnel",
                           #Plotting SurgicalOutcomesGG
                           plotOutput("SurgicalOutcomesGG")),
                           # dataTableOutput("table"),
                           # dataTableOutput("test"),
                           # dataTableOutput("test1"),
                           # textOutput("test2"),
                           # dataTableOutput("test3")),
                  #Tab 2: EWMA
                  tabPanel("Moving average",
                           plotOutput("SurgicalOutcomesGG2"))
    )
  )
  
)
)

##2. Shiny server
#Setting up Shiny server
server <- function(input, output, session) {
  
  #Setting observeEvent to update Facility select input as user selects LHD
  observeEvent(input$FacilityLHDName, {
    #Updating FacilityType
    updateSelectInput(session, "FacilityType",
                      choices = rev(sort(unique(data$FacilityType[data$FacilityLHDName == input$FacilityLHDName]))))
  })
  #Setting observeEvent to update FacilityName select input as user selects LHD and FacilityType
  observeEvent(c(input$FacilityType, input$FacilityLHDName), {
    #Updating FacilityName
    updateSelectInput(session, "FacilityName",
                      choices = sort(unique(data$FacilityName[data$FacilityType == input$FacilityType &
                                                                data$FacilityLHDName == input$FacilityLHDName])))
  })
  #Setting observeEvent to update Cancer select input as user selects LHD, FacilityType, and FacilityName
  observeEvent(c(input$FacilityType, input$FacilityLHDName, input$FacilityName), {
    #Updating CancerName
    updateSelectInput(session, "CancerName",
                      choices = sort(unique(data$CancerName[data$FacilityType == input$FacilityType &
                                                                data$FacilityLHDName == input$FacilityLHDName &
                                                                data$FacilityName == input$FacilityName])))
  })
  #Setting observeEvent to update IndicatorName select input as user selects LHD, FacilityType, FacilityName, and CancerName
  observeEvent(c(input$FacilityType, input$FacilityLHDName, input$FacilityName, input$CancerName), {
    #Updating 
    updateSelectInput(session, "IndicatorName",
                      choices = sort(unique(data$IndicatorName[data$FacilityType == input$FacilityType &
                                                              data$FacilityLHDName == input$FacilityLHDName &
                                                              data$FacilityName == input$FacilityName &
                                                              data$CancerName == input$CancerName])))
  })

  #Generating plots
  #Tab 1: Activity
  #Generating main reactive dataframe for Activity Tab
  rdfTab1Activity <- reactive({
    subset(data,
           FacilityLHDName == input$FacilityLHDName & 
             FacilityType == input$FacilityType &
             FacilityName == input$FacilityName &
             CancerName == input$CancerName &
             IndicatorName == input$IndicatorName
           )
    })
  #Generating reactive dataframe for NSW subset
  rdfTab1NSW = reactive({
    subset(data, 
           FacilityLHDName == "NSW" &
             FacilityType == input$FacilityType &
             FacilityName == "NSW" &
             CancerName == input$CancerName &
             IndicatorName == input$IndicatorName
           )
    })
  #Generating reactive NSWMean value
  NSWMean = reactive({rdfTab1NSW()$FacilityMeanRaw})
  #Generating reactive data frame for all other facilities for the same 
  rdfTab1FacilityOther = reactive({
    subset(data,
           FacilityType == input$FacilityType &
             FacilityName != "NSW" &
             CancerName == input$CancerName &
             IndicatorName == input$IndicatorName
    )
  })
  #reactive frame for all other within LHD
  rdfTab1FacilityOtherWithinLHD = reactive({
    subset(data,
           FacilityLHDName == input$FacilityLHDName &
           FacilityType == input$FacilityType &
             FacilityName != "NSW" &
             FacilityName != input$FacilityName &
             CancerName == input$CancerName &
             IndicatorName == input$IndicatorName
           )
    })

  #reactive frame for all other within LHD
  rdfTab1FacilityOtherOutsideLHD = reactive({
    subset(data,
           FacilityLHDName != input$FacilityLHDName &
           FacilityType == input$FacilityType &
             FacilityName != "NSW" &
             CancerName == input$CancerName &
             IndicatorName == input$IndicatorName
    )
  })


  # derive maximum x-axis value
  XMax = reactive({max(100, rdfTab1FacilityOther()$FacilityN * 1.1, na.rm=TRUE)})
  
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
  
  
  #Plotting funnel plot
  output$SurgicalOutcomesGG <-
    renderPlot({
      #Setting up validation to create custom error messages
      validate(
        #Validate that user has to input all data
        need(nrow(rdfTab1Activity())>0, "")
      )
      #Setting up funnel plot
      p <- ggplot(data = rdfTab1Activity()) +
        #Setting  x and y limits
        coord_cartesian(xlim=c(0,XMax()), ylim=c(0,1)) +
        #Y2SD polygon layer
        geom_polygon(data = fMakeFunnelPolygons(NSWMean(), XMax(), 0, 1), aes(x = X, y = Y2SD), fill = "#BEBEBE70", colour = NA) +
        #Y3SD polygon layer
        geom_polygon(data = fMakeFunnelPolygons(NSWMean(), XMax(), 0, 1), aes(x = X, y = Y3SD), fill = "#BEBEBE70", colour = NA) +
        #NSW mean layer
        geom_hline(yintercept = NSWMean(), colour="dodgerblue2") +
        #Rug layer of all other facilities
        geom_rug(data=rdfTab1FacilityOther(), aes(FacilityN), colour="darkgrey") +
        #Adding in labels for plot
        labs (x = "Total volume 2014-17", y="Proportion")
      #Default plot
      p <- p + 
        #Adjusted mean layer of facility of interest 
        geom_point(data = rdfTab1Activity(), aes(x=FacilityN, y=FacilityMeanMarg, color = "Adjusted mean"), size=4) +
        #Raw mean layer of facility of interest with 
        geom_point(data = rdfTab1Activity(), aes(x=FacilityN, y=FacilityMeanRaw, color = "Raw mean"), size=4) +
        #Line layer conneting points of adjusted and raw mean
        geom_segment(data = rdfTab1Activity(), aes(x=FacilityN, y=FacilityMeanRaw, xend=FacilityN, yend=FacilityMeanMarg)) +
        scale_color_manual("Selected facility", values=c("Adjusted mean"=rgb(106,66,137, maxColorValue = 255),
                                     "Raw mean" = rgb(245,130,32, maxColorValue = 255)
        ))
      #Additional plots conditional on checkbox AllOtherOutsideLHD
      #If AllOtherOutsideLHD checked, plot the following
      if (input$AllOtherOutsideLHD & !input$AllOtherWithinLHD & nrow(rdfTab1FacilityOtherOutsideLHD())>0 ) {
        #Default plot settings
        p <-  p +
          #Adjusted mean layer of Facilities outside LHD
          geom_point(data = rdfTab1FacilityOtherOutsideLHD(), aes(x=FacilityN, y=FacilityMeanMarg, fill = "Outside LHD"), size = 3, shape = 21) +
            scale_fill_manual("Other facilities", values = c(NA))
      }
      if (input$AllOtherWithinLHD & !input$AllOtherOutsideLHD & nrow(rdfTab1FacilityOtherWithinLHD())>0) {
      p <-  p +
        geom_point(data = rdfTab1FacilityOtherWithinLHD(), aes(x=FacilityN, y=FacilityMeanMarg, fill = "Within LHD"), size = 3, shape = 21, stroke = 1) +
        scale_fill_manual("Other facilities", values = c(rgb(0,171,230, maxColorValue = 255)))
      }
      if (input$AllOtherWithinLHD & input$AllOtherOutsideLHD & nrow(rdfTab1FacilityOtherWithinLHD())>0 & nrow(rdfTab1FacilityOtherOutsideLHD())>0) {
        p <-  p +
          geom_point(data = rdfTab1FacilityOtherOutsideLHD(), aes(x=FacilityN, y=FacilityMeanMarg, fill = "Outside LHD"), size = 3, shape = 21) +
          geom_point(data = rdfTab1FacilityOtherWithinLHD(), aes(x=FacilityN, y=FacilityMeanMarg, fill = "Within LHD"), size = 3, shape = 21) +
          scale_fill_manual("Other facilities", values = c(NA,  rgb(0,171,230, maxColorValue = 255)))
      }
      if (input$AllOtherWithinLHD & input$AllOtherOutsideLHD & nrow(rdfTab1FacilityOtherWithinLHD())==0 & nrow(rdfTab1FacilityOtherOutsideLHD())>0) {
        p <-  p +
          geom_point(data = rdfTab1FacilityOtherOutsideLHD(), aes(x=FacilityN, y=FacilityMeanMarg, fill = "Outside LHD"), size = 3, shape = 21) +
          scale_fill_manual("Other facilities", values = c(NA,  rgb(0,171,230, maxColorValue = 255)))
      }
      if (input$AllOtherWithinLHD & input$AllOtherOutsideLHD & nrow(rdfTab1FacilityOtherWithinLHD())>0 & nrow(rdfTab1FacilityOtherOutsideLHD())==0) {
        p <-  p +
          geom_point(data = rdfTab1FacilityOtherWithinLHD(), aes(x=FacilityN, y=FacilityMeanMarg, fill = "Within LHD"), size = 3, shape = 21) +
          scale_fill_manual("Other facilities", values = c(NA,  rgb(0,171,230, maxColorValue = 255)))
      }
      p
      })
  
  # , color = rgb(106,66,137, maxColorValue = 255)
  # , color = rgb(245,130,32, maxColorValue = 255), alpha = 0.75
  #Tab 2: EWMA
  #Generating main reactive dataframe for EWMA Tab
  rdfTab2EWMA <- reactive({data1 %>%
    filter(CancerName == input$CancerName,
           FacilityName == input$FacilityName,
           IndicatorName == input$IndicatorName) %>%
    na.omit()})


  #Setting maximum Y axis range
  YMax = 1

  #Plotting EWMA plot
  output$SurgicalOutcomesGG2 <-
    renderPlot({
      #Setting up validation to create custom error messages
      validate(
        #Generate custom error message if no dataframe available
        need(nrow(rdfTab2EWMA())>0, "The selected facility does not have sufficient data points for plotting")
        )
      #Setting plot resolution
      #Plotting EWMA plot
      ggplot(data = rdfTab2EWMA()) +
        #Setting  x and y limits
        coord_cartesian(xlim=c(0,40), ylim=c(0,YMax)) +
        #Layer ribbon (area between EWMA2SigmaLow and EWMA2SigmaHigh)
        geom_ribbon(aes(ymin = EWMA2SigmaLow, ymax = EWMA2SigmaHigh, x=EWMAX2), alpha = 0.5, fill = "#BEBEBE70") +
        #Layer for line plot EWMA2SigmaLow
        # geom_line(aes(x = EWMAX2, y = EWMA2SigmaLow)) +
        # #Layer for line plot EWMA2SigmaHigh
        # geom_line(aes(x = EWMAX2, y = EWMA2SigmaHigh)) +
        #Layer for line plot EWMAY
        geom_line(aes(x = EWMAX2, y = EWMAY), size=0.5, linetype = "dashed") +
        #Layer for point EWMAY
        geom_point(aes(x = EWMAX2, y = EWMAY), size = 3, colour = rgb(106,66,137, maxColorValue = 255)) +
        #Replacing x labels with YYYYQ
        scale_x_continuous(labels = rdfTab2EWMA()$XLabel, breaks = rdfTab2EWMA()$EWMAX2) + 
        #Rotating x axis labels
        theme(axis.text.x=element_text(angle=90,hjust=1)) +
        #Naming x and y axis
        labs (x = "Quarter 2008-17", y="Proportion")
      })
    
}

#Run App
shinyApp(ui, server)

