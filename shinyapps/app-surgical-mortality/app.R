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

#Loading data
data <- readRDS(file = "./data/SurgicalOutcomesAppDF.Rda")
data1 <- readRDS(file = "./data/SurgicalOutcomesAppDF2.Rda")
data$Count=1


#User functions
#Reordering layers
insertLayer <- function(P, after=0, ...) {
  #  P     : Plot object
  # after  : Position where to insert new layers, relative to existing layers
  #  ...   : additional layers, separated by commas (,) instead of plus sign (+)
  
  if (after < 0)
    after <- after + length(P$layers)
  
  if (!length(P$layers))
    P$layers <- list(...)
  else 
    P$layers <- append(P$layers, list(...), after)
  
  return(P)
}

###Building Shiny App
# .nav-tabs {border-bottom: 1px solid rgb(106,66,137);} table.dataTable {line-height:0.7}

##1. Shiny UI ns
#Setting ui as a fluidPage: FluidPage enables Shiny Apps to adjust to screens of varying resolution
ui <- fluidPage(title="Surgical Outcomes App",
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
# tags$head(tags$style(HTML("
#                           #FunnelPlotText1 {font-family: 'Helvetica Neue',Helvetica,Arial,sans-serif; 
#                           border: solid rgb(106,66,137) 2px;
#                           padding-top: 10px;
#                           padding-bottom: 10px;
#                           padding-left: 10px;
#                           padding-right: 10px;}"))),
  #Adding in titlePanel
  titlePanel(div(HTML("<strong><font style = 'color: rgb(106, 66, 137)'>Surgical Outcomes App</strong></font>"),
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
                  label = "Facility type:",
                  choices = rev(sort(unique(data$FacilityType)))
      ),
      #Select input field for Facility
      selectInput(input = "FacilityName",
                  label = "Facility name:",
                  choices = sort(unique(data$FacilityName))
      ),
      #Select input field for Cancer
      selectInput(input = "CancerName",
                  label = "Cancer:",
                  choices = sort(unique(data$CancerName))
      ),
      #Select input slider for year
      selectInput(input = "IndicatorName",
                  label = "Indicator:",
                  choices = sort(unique(data$IndicatorName))
      ),
      #Checkbox for AllOtherOutsideLHD
      conditionalPanel(condition = "input.tabpanel == 1", checkboxInput("AllOtherOutsideLHD", label = "All other outside LHD", value = FALSE)),
      #Checkbox for AllOtherWithinLHD
      conditionalPanel(condition = "input.tabpanel == 1", checkboxInput("AllOtherWithinLHD", label = "All other within LHD", value = FALSE))
    ),
    #Main panel
    mainPanel(
      #Creating tabs for main panel
      tabsetPanel(id = "tabpanel",
        type = "tabs",
                  #Tab 1: Activity
                  tabPanel("Funnel",
                           #Plotting SurgicalOutcomesGG
                           plotlyOutput("SurgicalOutcomesGG",  height = 450),
                           htmlOutput("FunnelPlotText1"),
                           value = 1),
                           # dataTableOutput("table"),
                           # dataTableOutput("test"),
                           # dataTableOutput("test1"),
                           # textOutput("test2"),
                           # dataTableOutput("test3")),
                  #Tab 2: EWMA
                  tabPanel("Moving average",
                           plotOutput("SurgicalOutcomesGG2", height = 450)),
                  #Tab 3: Data
                  tabPanel("Data",
                           htmlOutput("DataText")),
                  #Tab 4 Notes
                  tabPanel("Notes",
                           htmlOutput("NotesText"))
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
    renderPlotly({
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
        geom_hline(yintercept = NSWMean(), colour="dodgerblue2", aes(label = "")) +
        #Rug layer of all other facilities
        geom_rug(data=rdfTab1FacilityOther(), aes(FacilityN), colour="darkgrey") +
        #Adding in labels for plot
        labs (x = "Total volume 2014-17", y="Proportion")
      #Default plot
      p <- p +
        #Adjusted mean layer of facility of interest
        geom_point(data = rdfTab1Activity() %>%
                     select(FacilityMeanRaw, FacilityN, FacilityName) %>%
                     mutate(Group = "Raw Mean") %>%
                     rename(FacilityMean = FacilityMeanRaw) %>%
                     rbind(rdfTab1Activity() %>%
                             select(FacilityMeanMarg, FacilityN, FacilityName) %>%
                             rename(FacilityMean = FacilityMeanMarg) %>%
                             mutate(Group = "Adjusted Mean"))
                   ,aes(text=paste(FacilityName, ' (',Group,')', sep = ""), x=FacilityN, y=FacilityMean, color = Group), size = 2) +
        #Line layer conneting points of adjusted and raw mean
        geom_segment(data = rdfTab1Activity(), aes(x=FacilityN, y=FacilityMeanRaw, xend=FacilityN, yend=FacilityMeanMarg)) +
        scale_color_manual("Selected facility", values=c(rgb(106,66,137, maxColorValue = 255),
                                                         rgb(245,130,32, maxColorValue = 255))) +
        theme(legend.text=element_text(size=9))
      #Additional plots conditional on checkbox AllOtherOutsideLHD
      #If AllOtherOutsideLHD checked, plot the following
      if (input$AllOtherOutsideLHD & !input$AllOtherWithinLHD) {
        #Default plot settings
        p <- insertLayer(p, after = 3, (geom_point(data = rdfTab1FacilityOtherOutsideLHD(), aes(text=paste(FacilityName), x=FacilityN, y=FacilityMeanMarg, fill = "Outside LHD"), size = 2, shape = 21)))
        p <- p + scale_fill_manual("Other facilities", values = c(NA))
      } 
      else if (input$AllOtherWithinLHD & !input$AllOtherOutsideLHD) {
        p <-  insertLayer(p, after = 3, (geom_point(data = rdfTab1FacilityOtherWithinLHD() %>%
                                                         mutate(Group = "Within LHD"), aes(text=paste(FacilityName), x=FacilityN, y=FacilityMeanMarg, fill = Group), size = 2, shape = 21, stroke = 1)))
        p <- p + scale_fill_manual("Other facilities", values = c(rgb(0,171,230, maxColorValue = 255)))
        } else if (input$AllOtherWithinLHD & input$AllOtherOutsideLHD) {
        p <- insertLayer(p, after = 3, geom_point(data = rdfTab1FacilityOtherOutsideLHD() %>%
                                          mutate(Group = "Outside LHD"),
                                        aes(text=paste(FacilityName), x=FacilityN, y=FacilityMeanMarg, fill = Group), size = 2, shape = 21))
        p <- insertLayer(p, after = 3,geom_point(data = rdfTab1FacilityOtherWithinLHD() %>%
                       mutate(Group = "Within LHD"),
                     aes(text=paste(FacilityName), x=FacilityN, y=FacilityMeanMarg, fill = Group), size = 2, shape = 21))
        p <- p + scale_fill_manual("Other facilities", values = c(NA,  rgb(0,171,230, maxColorValue = 255)))
        }
      p <- ggplotly(p, tooltip="text") %>%
        layout(showlegend = FALSE)
    })
  
  output$FunnelPlotText1 <-renderText({paste("<b>Observations</b> <br>", 
                                             "Between 2014-17 ",input$FacilityName," had ", rdfTab1Activity()$FacilityN," ", input$CancerName ," procedures. <br>",
                                             "For the same time period, the adjusted and raw proporiton of ", input$IndicatorName, " for ", input$FacilityName, " were ", round(rdfTab1Activity()$FacilityMeanMarg*100, 2), "%", " and ",
                                             round(rdfTab1Activity()$FacilityMeanRaw*100, 2), "% respectively.",
                                             "<br>","<br>",
                                             "<b>Insights</b> <br>",
                                             "Any insights listed here…", sep = "")})
  
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
  
  #Tab 3: Data
  output$DataText <- renderText(paste("Data goes here – if appropriate"))
  # , colourScale=my_color
  
  #Tab 4: Notes
  output$NotesText <- renderText(paste("Lorem ipsum dolor amet mlkshk ugh sartorial shabby chic schlitz XOXO portland fam ethical sriracha swag af. Plaid try-hard forage lomo sustainable. Fam fixie distillery succulents +1 crucifix tacos, keffiyeh aesthetic cardigan. Activated charcoal godard air plant, helvetica chambray bicycle rights YOLO pickled squid. Marfa affogato twee humblebrag."))
}




#Run App
shinyApp(ui, server)

