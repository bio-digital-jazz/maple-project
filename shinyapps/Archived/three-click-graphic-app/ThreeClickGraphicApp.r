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
## Fun plot
# grab some data
Image1 = png::readPNG("Drugs2.png")
# Image1 = jpeg::readJPEG("Drugs3.jpg")

Rast1  = grid::rasterGrob(Image1, interpolate = T, height = 1, width = 1)
Line = data.frame(Year = 2009:2019,
                  Spend = 6:16 + rnorm(11))
Poly = rbind(c(0,0), Line, c(2020, 100), c(0,100))





###Building Shiny App

##1. Shiny UI ns
#Setting ui as a fluidPage: FluidPage enables Shiny Apps to adjust to screens of varying resolution
ui <- fluidPage(title="Infographic", column(12,offset = 0,
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
                                Table {padding-top: 10px;}
                                "
                )
                ),
                tags$head(tags$style(
                  type="text/css",
                  "#Infographic img, {width: 60%;
                   display: block;
                   margin-left: auto;
                   margin-right: auto;}")),
                #Adding in titlePanel
                titlePanel(div(HTML("<strong><font style = 'color: rgb(106, 66, 137)'>Radiotherapy Distance App</strong></font>"),
                               img(src="CILogo.jpg", align ="right", height=55))),
                #Main panel
                mainPanel(width = 12,
                    # Creating tabs for main panel
                    column(width = 12),
                    tabsetPanel(id = "tabpanel",
                                position = "centre",
                                type = "tabs",
                                #Tab 1: Radiotherapy Distance Levels
                                tabPanel("Infographic",
                                         br(),
                                         #Plotting Radiotherapy Distance Levels
                                         div(plotOutput("Infographic"), align = "center"),
                                         value = 1
                                         ),
                                #Tab 2: Radiotherapy Distance Proportions
                                tabPanel("Graph",
                                         br(),
                                         #Plotting Radiotherapy Distance Proportions
                                         # plotOutput("RadiotherapyDistanceProportion", height = 450),
                                         value = 2
                                ),
                                #Tab 3: Map
                                tabPanel("Map",
                                         #Plotting Radiotherapy Distance Map
                                         # plotOutput("RadiotherapyDistanceMap"),
                                         value = 3)
                    )
                )
)
)


##2. Shiny server
#Setting up Shiny server
server <- function(input, output, session) {
  
  
  #Plotting visualisations
  #Tab 1: Radiotherapy Distance Levels
  # Tab1: Infographic
  output$Infographic = renderPlot({ggplot() +
    annotation_custom(Rast1, ymin = 0, ymax = 20, xmin = 2009) +
    coord_cartesian(xlim=c(2009,2018), ylim=c(4,20)) +
    geom_line(data=Line, aes(x=Year, y=Spend), size=10, colour="red") +
    geom_polygon(data=Poly, aes(x=Year, y=Spend), fill="white") + 
    labs(title="Drug prices soaring!", subtitle="Infographic",
         x="Year", y="Annual drug spend ($ million)",
         caption="Source: PBAC etc")
    }  , height = 800, width = 800)
  
  # 

  
  # #Tab 2: Radiotherapy Distance Proportion
  # output$RadiotherapyDistanceProportion <- renderPlot({ggplot() + 
  #     geom_bar(data = RadioTXDF4() %>%
  #                filter(!is.na(RadioTXDF4()$ResidentLHDName)), 
  #              aes(x = ResidentLHDName, weight = Prop, fill = factor(DistanceExcess1Cat, levels=c("500km+", "251-500km", "101-250km",  "51-100km",  "11-50km", "Nearest"), ordered = TRUE))) +
  #     ggplot2:::manual_scale('fill', values = setNames(kBarCols,
  #                                                      c("Nearest", "11-50km",  "51-100km", "101-250km", "251-500km", "500km+"))) + coord_flip() + labs(fill="Distance")  + ggtitle(paste0("Excess travel: ", input$CancerCode)) + 
  #     theme(plot.title = element_text(size=18)) + 
  #     scale_y_continuous(labels = scales::percent) +
  #     theme(plot.title = element_text(size=18)) +
  #     xlab("") +
  #     ylab("Percent")
  # })
  # 
  # #Tab 3: Radiotherapy Distance Map
  # output$RadiotherapyDistanceMap = renderPlot({ggplot() + 
  #     coord_fixed(xlim=c(XMin(),XMax()), ylim=c(YMin(), YMax())) +
  #     geom_polygon(data=LHDPolygons, 
  #                  aes(x=Long, y=Lat, group=LHDSegment),
  #                  colour = NA, fill="lightgrey", size=0) +
  #     geom_polygon(data=LHDXPolygon(), aes(x=Long, y=Lat, group=LHDSegment),
  #                  colour = NA, fill="darkgrey", size=0) +
  #     geom_segment(data = RadioTXDF2(),
  #                  aes(x = ResidentLong, y = ResidentLat, xend = TXFacilityLong, yend = TXFacilityLat, colour=DistanceExcess1Cat),
  #                  alpha=0.5, size=1.25) +
  #     geom_polygon(data=LHDXPolygon(), aes(x=Long, y=Lat, group=LHDSegment),
  #                  colour = "white", fill=NA, size=0.5) +
  #     geom_point(data=RadioFacilities,
  #                aes(x=TXFacilityLong, y=TXFacilityLat), colour="red", size=1) + 
  #     facet_wrap(. ~ DistanceExcess1Cat, ncol = 2) +
  #     scale_color_manual(values = kBarCols, name = "Excess Travel") +
  #     theme(strip.background =element_rect(fill=kBarCols)) +
  #     theme(strip.text = element_text(colour = 'white', size = 12)) + 
  #     guides(colour=FALSE)
  #   
  # }) 
  }


#Run App
shinyApp(ui, server)

