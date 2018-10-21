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
Image1 = png::readPNG("./www/Drugs2.png")

Rast1  = grid::rasterGrob(Image1, interpolate = T, height = 1, width = 1)
Line = data.frame(Year = 2009:2019,
                  Spend = 6:16 + rnorm(11))
Poly = rbind(c(0,0), Line, c(2020, 100), c(0,100))


###Building Shiny App

##1. Shiny UI ns
#Setting ui as a fluidPage: FluidPage enables Shiny Apps to adjust to screens of varying resolution
ui <- fixedPage(title="Infographic", fixedRow(offset = 0,
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
                titlePanel(div(HTML("<strong><font style = 'color: rgb(106, 66, 137)'>Infographic</strong></font>"),
                               img(src="CILogo.jpg", align ="right", height=55))),
                #Main panel
                mainPanel(width = 12,
                    # Creating tabs for main panel
                    column(width = 12),
                    tabsetPanel(id = "tabpanel",
                                position = "centre",
                                type = "tabs",
                                #Tab 1: Infographic
                                tabPanel("Infographic",
                                         br(),
                                         #Plotting Radiotherapy Distance Levels
                                         div(plotOutput("Infographic"), align = "center"),
                                         value = 1
                                         ),
                                #Tab 2: Graph 1
                                tabPanel("Graph 1",
                                         br(),
                                         #Plotting Radiotherapy Distance Proportions
                                         plotOutput("FormalPlotCol", height = 450),
                                         value = 2
                                ),
                                #Tab 3: Table
                                tabPanel("Table",
                                         br(),
                                         #Plotting Radiotherapy Distance Map
                                         dataTableOutput("LineDF"),
                                         value = 3),
                                #Tab 4: Notes
                                tabPanel("Notes",
                                         #Plotting Radiotherapy Distance Map
                                         # plotOutput("RadiotherapyDistanceMap"),
                                         htmlOutput("notes"))
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
         caption="Source: PBAC etc") + 
      theme(plot.title = element_text(size=25, face = "bold"),
            plot.subtitle = element_text(size=15)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
    }  , height = 800, width = 800)
  
  # 

  
  #Tab 2: Graph 1
  output$FormalPlotCol <- renderPlot({ggplot() + 
        geom_col(data=Line, aes(x=Year, y=Spend), fill = rgb(106,66,137, maxColorValue = 256)) + 
        coord_cartesian(xlim=c(2008.5,2019), ylim=c(0,20)) +
        scale_x_discrete(limits=2009:2018) +
        labs(title="Drug prices increasing over time", subtitle="Publication quality plot",
             x="Year", y="Annual drug spend ($ million)",
             caption="Source: PBAC etc") +
      theme(plot.title = element_text(size=20, face = "bold"),
            plot.subtitle = element_text(size=12))
  })

  #Tab 3: Table
  output$LineDF <- renderDataTable({Line
    })
  
  # #Tab 4: Notes
  output$notes<- renderUI({
    str1<-paste("Data Source = XXX")
    str2<-paste("This is a summary of annual drug spend for cancer drugs by NSW etc")
    str3<-paste("Cancer Institute NSW 2018")
    url <- a("https://www.cancer.nsw.gov.au/", href="https://www.cancer.nsw.gov.au/")
    str4<-paste("For further info go to", tagList("URL link:", url))
    HTML(paste(str1, str2, str3, str4, sep = '<br/>'))})
  }

#Run App
shinyApp(ui, server)

