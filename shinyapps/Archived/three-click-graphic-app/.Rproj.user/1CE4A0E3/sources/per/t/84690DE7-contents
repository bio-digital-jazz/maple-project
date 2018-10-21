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
library(plotly)
library(Cairo)
options(shiny.usecairo=T)


### Constants - colours from RBCO chart
kColPurple = "#48156BBF"
kBarCols = c(
  rgb( 74/256,  37/256, 116/256),
  rgb(168/256, 143/256, 190/256),
  rgb(128/256,  39/256, 122/256),
  rgb(206/256, 178/256, 205/256),
  rgb(  9/256, 168/256, 224/256),
  rgb(  0/256,  86/256, 161/256),
  rgb(134/256, 151/256, 204/256))
  

### Import & Tidy & Transform

#Importing radiotherapy dataset
RadioTXDF0 = readRDS(file = "RadiotherapyDistanceApp.Rda")

#Derive a summary table of radio facilities
RadioFacilities = RadioTXDF0 %>%
  dplyr::group_by(TXFacilityName, TXFacilityLong, TXFacilityLat) %>%
  dplyr::summarise() %>%
  na.omit()

#Importing LHD polygons
LHDPolygons = readr::read_csv("LHDMapCoords.csv")


###Building Shiny App

##1. Shiny UI ns
#Setting ui as a fluidPage: FluidPage enables Shiny Apps to adjust to screens of varying resolution
ui <- fluidPage(title="Radiotherapy Distance App",
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
                titlePanel(div(HTML("<strong><font style = 'color: rgb(106, 66, 137)'>Radiotherapy Distance App</strong></font>"),
                               img(src="CILogo.jpg", align ="right", height=55))),
                #Setting overall layout to be sidebar layout
                sidebarLayout(
                  #Inputs to visualisation
                  #Sidebar panel
                  sidebarPanel(
                    #Select input field for CancerCode
                    selectInput(input = "CancerCode",
                                label = "Cancer code: ",
                                choices = sort(unique(RadioTXDF0$Cancer))
                    ),
                    #Conditional for LHDName select input to appear only when on the Map tab
                    conditionalPanel(
                      condition = "input.tabpanel == 3", selectInput(input = "LHDName",
                                                                     label = "Local health district: ",
                                                                     choices = sort(unique(RadioTXDF0$ResidentLHDName))
                      )
                    )
                  ),
                  #Main panel
                  mainPanel(
                    #Creating tabs for main panel
                    tabsetPanel(id = "tabpanel",
                                type = "tabs",
                                #Tab 1: Radiotherapy Distance Levels
                                tabPanel("Radiotherapy Distance Levels",
                                         br(),
                                         #Plotting Radiotherapy Distance Levels
                                         plotOutput("RadiotherapyDistanceLevels",  height = 450),
                                         value = 1
                                         ),
                                #Tab 2: Radiotherapy Distance Proportions
                                tabPanel("Radiotherapy Distance Proportion",
                                         br(),
                                         #Plotting Radiotherapy Distance Proportions
                                         plotOutput("RadiotherapyDistanceProportion", height = 450),
                                         value = 2
                                ),
                                #Tab 3: Map
                                tabPanel("Map",
                                         #Plotting Radiotherapy Distance Map
                                         plotOutput("RadiotherapyDistanceMap"),
                                         value = 3)
                                )
                    )
                  )
                )


##2. Shiny server
#Setting up Shiny server
server <- function(input, output, session) {
  
  #Setting observeEvent to update Facility select input as user selects LHD
  observeEvent(input$CancerCode, {
    #Updating FacilityType
    updateSelectInput(session, "LHDName",
                      choices = (sort(unique(RadioTXDF0$ResidentLHDName[RadioTXDF0$Cancer == input$CancerCode]))))
    })
  
  #Generating reactive dataframes for plots
  #Reactive dataframe to subset CancerCodes
  RadioTXDF1 <- reactive({
    subset(RadioTXDF0,
           Cancer == input$CancerCode
    )
  })
  
  #Reactive dataframe to generate cancer specific and LHD specific dataframe
  RadioTXDF2 <- reactive({
    subset(RadioTXDF1(),
           ResidentLHDName == input$LHDName)
  })

  #Reactive dataframe to generate counts of patients by ResidentLHDName and DistanceExcess1Cat
  RadioTXDF3 = reactive({RadioTXDF1() %>%
      group_by(ResidentLHDName, DistanceExcess1Cat) %>%
      summarize(Freq = n()) %>%
      ungroup()
  })

  #Reactive dataframe to generate counts of patients by ResidentLHDName and DistanceExcess1Cat
  RadioTXDF4 = reactive({RadioTXDF3() %>%
      group_by(ResidentLHDName) %>%
      mutate(Total = sum(Freq)) %>%
      ungroup() %>%
      mutate(Prop = Freq/Total)})

  #Reactive LHD specific polygon for map
  LHDXPolygon = reactive({LHDPolygons %>%
    filter(LHDName==input$LHDName)})
  
  #Reactive plot coords based on LHD and journeys to set the plot region for the map
  XMax = reactive({max(LHDXPolygon()$Long, RadioTXDF2()$ResidentLong, RadioTXDF2()$TXFacilityLong, na.rm=TRUE)})
  XMin = reactive({min(LHDXPolygon()$Long, RadioTXDF2()$ResidentLong, RadioTXDF2()$TXFacilityLong, na.rm=TRUE)})
  XRange = reactive({XMax() - XMin()})
  YMax = reactive({max(LHDXPolygon()$Lat, RadioTXDF2()$ResidentLat, RadioTXDF2()$TXFacilityLat, na.rm=TRUE)})
  YMin = reactive({min(LHDXPolygon()$Lat, RadioTXDF2()$ResidentLat, RadioTXDF2()$TXFacilityLat, na.rm=TRUE)})
  YRange = reactive({YMax() - YMin()})
  
  reactive({if (XRange() / 1.3 > YRange()) {
    YRange = XRange() / 1.3
  } else {
    XRange = YRange() * 1.3
  }})
  
  
  #Plotting visualisations
  #Tab 1: Radiotherapy Distance Levels
  output$RadiotherapyDistanceLevels <- renderPlot({ggplot() + 
      geom_bar(data = RadioTXDF3() %>%
                 filter(!is.na(RadioTXDF3()$ResidentLHDName)), 
               aes(x = ResidentLHDName, weight = Freq, fill = factor(DistanceExcess1Cat, levels=c("500km+", "251-500km", "101-250km",  "51-100km",  "11-50km", "Nearest"), ordered = TRUE))) +
      ggplot2:::manual_scale('fill', values = setNames(kBarCols,
                                                       c("Nearest", "11-50km",  "51-100km", "101-250km", "251-500km", "500km+"))) + 
      labs(fill="Distance") + 
      ggtitle(paste0("Excess travel: ", input$CancerCode)) + 
      theme(plot.title = element_text(size=18)) + 
      xlab("") +
      ylab("Count") +
      coord_flip()
    })
  
  #Tab 2: Radiotherapy Distance Proportion
  output$RadiotherapyDistanceProportion <- renderPlot({ggplot() + 
      geom_bar(data = RadioTXDF4() %>%
                 filter(!is.na(RadioTXDF4()$ResidentLHDName)), 
               aes(x = ResidentLHDName, weight = Prop, fill = factor(DistanceExcess1Cat, levels=c("500km+", "251-500km", "101-250km",  "51-100km",  "11-50km", "Nearest"), ordered = TRUE))) +
      ggplot2:::manual_scale('fill', values = setNames(kBarCols,
                                                       c("Nearest", "11-50km",  "51-100km", "101-250km", "251-500km", "500km+"))) + coord_flip() + labs(fill="Distance")  + ggtitle(paste0("Excess travel: ", input$CancerCode)) + 
      theme(plot.title = element_text(size=18)) + 
      scale_y_continuous(labels = scales::percent) +
      theme(plot.title = element_text(size=18)) +
      xlab("") +
      ylab("Percent")
  })
  
  #Tab 3: Radiotherapy Distance Map
  output$RadiotherapyDistanceMap = renderPlot({ggplot() + 
      coord_fixed(xlim=c(XMin(),XMax()), ylim=c(YMin(), YMax())) +
      geom_polygon(data=LHDPolygons, 
                   aes(x=Long, y=Lat, group=LHDSegment),
                   colour = NA, fill="lightgrey", size=0) +
      geom_polygon(data=LHDXPolygon(), aes(x=Long, y=Lat, group=LHDSegment),
                   colour = NA, fill="darkgrey", size=0) +
      geom_segment(data = RadioTXDF2(),
                   aes(x = ResidentLong, y = ResidentLat, xend = TXFacilityLong, yend = TXFacilityLat, colour=DistanceExcess1Cat),
                   alpha=0.5, size=1.25) +
      geom_polygon(data=LHDXPolygon(), aes(x=Long, y=Lat, group=LHDSegment),
                   colour = "white", fill=NA, size=0.5) +
      geom_point(data=RadioFacilities,
                 aes(x=TXFacilityLong, y=TXFacilityLat), colour="red", size=1) + 
      facet_wrap(. ~ DistanceExcess1Cat, ncol = 2) +
      scale_color_manual(values = kBarCols, name = "Excess Travel") +
      theme(strip.background =element_rect(fill=kBarCols)) +
      theme(strip.text = element_text(colour = 'white', size = 12)) + 
      guides(colour=FALSE)
    
  }) 
  }


#Run App
shinyApp(ui, server)

