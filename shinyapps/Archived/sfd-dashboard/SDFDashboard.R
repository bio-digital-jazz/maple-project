library(fpp2)
library(tibble)
library(ggplot2)
library(png)
library(flexdashboard)
library(shiny)
library(tidyverse)
library(stringr)
library(plotly)
library(scales)
library(lubridate)
library(Cairo)
library(shinydashboard)
library(zoo)




##########################################
### Set date
CurrentYear <- as.numeric(format(Sys.Date(), "%Y"))
CurrentMon <- as.numeric(format(Sys.Date(), "%m"))
CurrentDay <- as.numeric(format(Sys.Date(), "%d"))




## Top level data
Statedata <- list("./Data/TopDashboard1MonDataDF.Rda", "./Data/TopDashboard2MonDataTS.Rda")
lapply(Statedata,load,.GlobalEnv)

StateDF <- TopMonDataDF
StateTS <- TopMonDataTS


## SAS level data
SASlist <- list("GSSW", "GW" , "HNE", "NC", "NSCC", "SESI", "SWS", "SYD", "SW")
SASdata <- list("./Data/SASDashboard1MonDataDF.Rda", "./Data/SASDashboard2MonDataTS.Rda")
lapply(SASdata,load,.GlobalEnv)




## Age level data
AgeList <- list( "40-49"= "4049" , "50-69" = "5069", "70-74" = "7074", "75 plus" = "75")
AgeData <- list("./Data/AgeDashboard1MonDataDF.Rda", "./Data/AgeDashboard2MonDataTS.Rda")
lapply(AgeData,load,.GlobalEnv)


#############################################
###### UI
ui <- fluidPage(
  tags$style(HTML("
                  .box.box-solid.box-primary{
                  background:#F3F3F4
                  }
                  ")),
  box(width = 12, height = 67, status = "primary", solidHeader = TRUE,
      fluidRow(column(width=2, align="left", img(src = "Top.png")))),
  
  tabsetPanel( 
    tabPanel(h4("Forecast"), fluid = TRUE, 
             h3("Breast Screen Forecasting Dashboard"),              
             sidebarLayout(
               sidebarPanel (
                 radioButtons("response1", "Forecasting level", choices = list ("State" = "State",  "SAS" = "SAS", "Age group" = "Age"),selected = "State"), br(), br(),
                 conditionalPanel(
                   condition = "input.response1 == 'State'",
                   column(6, selectInput("State1", "", choices = list( "Total screens" ="Total_Screeners", "Inital screens"  = "Initial_Screeners",
                                                                       "Re-screen" = "Re_Screeners", "Arabic screeners"= "Arabic_Screeners",
                                                                       "ATSI screeners" = "ATSI_Screeners", "NESB screeners" = "NESB_Screeners"),
                                         selected = "Total_Screeners"))),
                 conditionalPanel(
                   condition = "input.response1 == 'SAS'",
                   column(6, selectInput("SAS_code1", "", choices = SASlist,
                                         selected = "HNE"))),
                 conditionalPanel(
                   condition = "input.response1 == 'Age'",
                   column(6, selectInput("AgeGroup1", "", choices = AgeList,
                                         selected = "5069")))
                 
                 
                 
                 
               ),
               
               mainPanel(
                 span(textOutput(outputId = "plotDescription"),  style = "font-family: 'verdana'; font-size: 8pt;font-weight: bold; color:#58595B"),  br(), br(),
                 tabsetPanel(type = "tabs",
                             # tab showing bookings forecast 
                             tabPanel(
                               title = "Bookings", id = "bookingTab",
                               
                               
                               plotlyOutput(outputId = "plotBooking")
                             ),
                             # tab showing screens forecast
                             tabPanel(
                               title = "Screens", id = "screenTab",
                               
                               
                               plotlyOutput(outputId = "plotScreen1")
                             )
                 )
               )
             )
    ), 
    tabPanel( h4("Time-Series"), fluid = TRUE, 
              h3("Breast Screen Forecasting Dashboard"),
              sidebarLayout(
                sidebarPanel(
                  helpText(h4("Please choose the analysis break down level:")),
                  radioButtons("response2", "", choices = list ( "State" = "State",  "SAS" = "SAS", "Age group" = "Age"), selected = "State"),
                  
                  conditionalPanel(
                    condition = "input.response2 == 'SAS'", 
                    column(6, selectInput("SAS_code2", "", choices = SASlist,
                                          selected = "HNE"))),
                  
                  conditionalPanel(
                    condition = "input.response2 == 'Age'", 
                    column(6, selectInput("AgeGroup2", "", choices = AgeList,
                                          selected = "5069"))),
                  
                  br(), br(), br(),
                  helpText(h4("Create time series plot for:")),
                  radioButtons("TS2","", choices = list ( "Invitations", "Slots" = "Total_Slots", "Appointment" = "Total_Bookings","Screens" = "Total_Screeners"),
                               selected = "Total_Screeners")
                  
                  
                  
                  
                  
                ),
                
                mainPanel(
                  
                  plotlyOutput(outputId = "plotTS")
                  
                ))
    )
    
  ))

################################################
#### server

server <-  function(input, output) {
  
  output$plotDescription <- renderText({
    if (input$response1 == "State") {paste("You have selected", input$response1, input$State1)}
    else if (input$response1 == "SAS"){paste( "You have selected", input$SAS_code1, input$response1 )}
    else {paste( "You have selected", input$response1, input$AgeGroup1 )}
    
  })
  
  
  output$plotBooking <- renderPlotly({
    
    ### reading data
    response <- input$response1
    
    if (response == "State"){
      
      currentyeardata_DF <- StateDF[StateDF$Year == CurrentYear,]
    }
    else if (response == "SAS"){
      
      SAS_DF <- as.data.frame (SASDashboard1MonDataDF[input$SAS_code1])
      colname <- c("Month", "Year", "Total_Screeners", "Target", "Initial_Screeners", "Re_Screeners", 
                   "Total_Bookings", "Total_Slots" , "Total_Dues", "NSW_Population", "ATSI_Population", "NESB_Population",
                   "Initial_First","Initial_Second","Initial_DNR1", "Initial_DNR2", "Initial_DNRDNR", "NSW_Business_Days", 
                   "NSW_Business_Days_UnAdj", "Campaign", "CampaignNot2018","Biannual", "Easter", "UnusualUp", "UnusualDown")
      colnames(SAS_DF) <- colname
      currentyeardata_DF <- SAS_DF[SAS_DF$Year == CurrentYear,]
    }
    else if (response == "Age"){
      
      Age_DF <- as.data.frame (AgeDashboard1MonDataDF[input$AgeGroup1])
      colname <- c("Month", "Year","Total_Screeners", "Initial_Screeners", "Re_Screeners", 
                   "Total_Bookings", "Total_Slots" , "Total_Dues", "NSW_Population", "ATSI_Population", "NESB_Population",
                   "Initial_First","Initial_Second","Initial_DNR1", "Initial_DNR2", "Initial_DNRDNR", "NSW_Business_Days", 
                   "NSW_Business_Days_UnAdj", "Campaign", "CampaignNot2018","Biannual", "Easter", "UnusualUp", "UnusualDown")
      colnames(Age_DF) <- colname
      currentyeardata_DF <- Age_DF[Age_DF$Year == CurrentYear,]
    }
    
    
    ### reading model
    if (input$response1 == "State" & input$State1 =="Total_Screeners"){
      model <- readRDS(file = "Total_Bookings")
      ActualScreens <- currentyeardata_DF[,"Total_Bookings"]
      
    }
    else if (input$response1 == "State" & input$State1 =="Initial_Screeners"){
      model <- readRDS(file = "Initial_Bookings")
      ActualScreens <- currentyeardata_DF[,"Initial_Bookings"]
    }
    else if (input$response1 == "State" & input$State1 =="Re_Screeners"){
      model <- readRDS(file = "Rescreen_Bookings")
      ActualScreens <- currentyeardata_DF[,"Rescreen_Bookings"]
      
    }
    else if (input$response1 == "State" & input$State1 =="NESB_Screeners"){
      model <- readRDS(file = "NESB_Bookings")
      ActualScreens <- currentyeardata_DF[,"NESB_Bookings"]
      
    }
    else if (input$response1 == "State" & input$State1 =="Arabic_Screeners"){
      model <- readRDS(file = "Arabic_Bookings")
      ActualScreens <- currentyeardata_DF[,"Arabic_Bookings"]
      
    }
    else if (input$response1 == "State" & input$State1 =="ATSI_Screeners"){
      model <- readRDS(file = "ATSI_Bookings")
      ActualScreens <- currentyeardata_DF[,"ATSI_Bookings"]
      
    }
    else if(input$response1 == "SAS"){
      model <- readRDS(file = paste0(input$SAS_code1, "Total_Screeners"))
      ActualScreens <- currentyeardata_DF[,"Total_Screeners"]
      
    }
    
    else if(input$response1 == "Age"){
      model <- readRDS(file = paste0(input$AgeGroup1, "Total_Screeners"))
      ActualScreens <- currentyeardata_DF[,"Total_Screeners"]
      
    }
    
    ###Time vector for forecast model #
    time_frc <- as.yearmon(time(window(StateTS, start = c(CurrentYear, 1), end = c(CurrentYear, 12))[,"Total_Screeners"]))
    time <- as.factor(time_frc)
    
    # forecast
    forecast <- forecast(model, newdata = currentyeardata_DF)
    fcast <- round(as.numeric(forecast$mean))
    
    
    if (input$response1 %in% c("State", "SAS")){
      PlotData <- data.frame( time, screenings = ActualScreens , target = round(currentyeardata_DF[,"Target"]), fcast)
    }
    else {PlotData <- data.frame( time, screenings = ActualScreens , fcast)
    }
    PlotData$time <- factor(PlotData$time, levels = PlotData$time)
    
    
    PlotData_past <- PlotData[1:(CurrentMon-1), ]
    PlotData_future <- PlotData[CurrentMon:12, ]
    
    cols <- c("Actual" = "#7A378B",  "Target" = "red", "Model" = "#00BFFF")
    
    p <- ggplot() +
      geom_bar(data = PlotData,aes(x = time, y = screenings),fill = "transparent", stat="identity",  width = 0.1)+
      geom_bar(data = PlotData_past,aes(x = time, y = screenings, fill = "Actual"), stat="identity",  width = 0.5) +
      geom_point(data= PlotData_future, aes(x = time, y = fcast, colour = "Model"), shape = 8, size = 2) +
      geom_line(data = PlotData_future, aes(x = time, y = fcast, colour = "Model", group = 1), lty = "dashed") +
      geom_segment(data = PlotData, aes(y = 0, yend = max(PlotData$screenings), x = CurrentMon - 0.5, xend = CurrentMon - 0.5), colour = "grey", lty = "dotted", size = 1) +
      ggtitle (paste(CurrentYear, "appointments forecast plot")) +
      xlab("Year") +
      ylab("Number of appointments") +
      theme_bw() +
      theme(text = element_text(family = "Verdana"),
            panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            axis.title.y = element_text(size = 8, colour = "#58595B", margin = margin(t = 10, r = 10, b = 100, l = 10)),
            axis.title.x = element_text(size = 8, colour = "#58595B", margin = margin(t = 7, r = 10, b = 0, l = 10)),
            axis.line = element_line(colour = "#58595B", size = .176),
            axis.ticks = element_line(colour = "#58595B", size = .176),
            axis.ticks.length =unit(1.25, "mm"),
            axis.text  = element_text(colour = "#58595B", size = 7),
            legend.title = element_blank(),
            legend.box = "horizontal",
            legend.text = element_text(colour = "#58595B", size = 7, lineheight = 1),
            legend.direction = "horizontal",
            legend.justification = "bottom",
            plot.title = element_text(size = 10,  face = "bold", colour = "#58595B")
            
      ) +
      scale_y_continuous(label = comma, expand = c(0,0)) +
      scale_colour_manual(name = "" , values = cols) +
      scale_fill_manual(name = "", values = cols)
    
    
    ggplotly()
    
  })
  
  output$plotScreen1 <- renderPlotly({
    
    ### reading data
    response <- input$response1
    
    if (response == "State"){
      
      currentyeardata_DF <- StateDF[StateDF$Year == CurrentYear,]
    }
    else if (response == "SAS"){
      
      SAS_DF <- as.data.frame (SASDashboard1MonDataDF[input$SAS_code1])
      colname <- c("Month", "Year","Total_Screeners", "Target", "Initial_Screeners", "Re_Screeners", 
                   "Total_Bookings", "Total_Slots" , "Total_Dues", "NSW_Population", "ATSI_Population", "NESB_Population",
                   "Initial_First","Initial_Second","Initial_DNR1", "Initial_DNR2", "Initial_DNRDNR", "NSW_Business_Days", 
                   "NSW_Business_Days_UnAdj", "Campaign", "CampaignNot2018","Biannual", "Easter", "UnusualUp", "UnusualDown")
      colnames(SAS_DF) <- colname
      currentyeardata_DF <- SAS_DF[SAS_DF$Year == CurrentYear,]
    }
    else if (response == "Age"){
      
      Age_DF <- as.data.frame (AgeDashboard1MonDataDF[input$AgeGroup1])
      colname <- c("Month", "Year","Total_Screeners", "Initial_Screeners", "Re_Screeners", 
                   "Total_Bookings", "Total_Slots" , "Total_Dues", "NSW_Population", "ATSI_Population", "NESB_Population",
                   "Initial_First","Initial_Second","Initial_DNR1", "Initial_DNR2", "Initial_DNRDNR", "NSW_Business_Days", 
                   "NSW_Business_Days_UnAdj", "Campaign", "CampaignNot2018","Biannual", "Easter", "UnusualUp", "UnusualDown")
      colnames(Age_DF) <- colname
      currentyeardata_DF <- Age_DF[Age_DF$Year == CurrentYear,]
    }
    
    
    ### reading model
    if (input$response1 == "State" & input$State1 %in% c("Total_Screeners", "Initial_Screeners", "Re_Screeners", "Arabic_Screeners", "ATSI_Screeners", "NESB_Screeners")){
      model <- readRDS(file = input$State1)
      ActualScreens <- currentyeardata_DF[,input$State1]
      
    }
    else if(input$response1 == "SAS"){
      model <- readRDS(file = paste0(input$SAS_code1, "Total_Screeners"))
      ActualScreens <- currentyeardata_DF[,"Total_Screeners"]
      
    }
    
    else if(input$response1 == "Age"){
      model <- readRDS(file = paste0(input$AgeGroup1, "Total_Screeners"))
      ActualScreens <- currentyeardata_DF[,"Total_Screeners"]
      
    }
    
    ###Time vector for forecast model #
    time_frc <- as.yearmon(time(window(StateTS, start = c(CurrentYear, 1), end = c(CurrentYear, 12))[,"Total_Screeners"]))
    time <- as.factor(time_frc)
    
    # forecast
    forecast <- forecast(model, newdata = currentyeardata_DF)
    fcast <- round(as.numeric(forecast$mean))
    
    
    if (input$response1 %in% c("State", "SAS")){
      PlotData <- data.frame( time, screenings = ActualScreens , target = round(currentyeardata_DF[,"Target"]), fcast)
    }
    else {PlotData <- data.frame( time, screenings = ActualScreens , fcast)
    }
    PlotData$time <- factor(PlotData$time, levels = PlotData$time)
    
    
    PlotData_past <- PlotData[1:(CurrentMon-1), ]
    PlotData_future <- PlotData[CurrentMon:12, ]
    
    cols <- c("Actual" = "#7A378B",  "Target" = "red", "Model" = "#00BFFF")
    
    p <- ggplot() +
      geom_bar(data = PlotData,aes(x = time, y = screenings),fill = "transparent", stat="identity",  width = 0.1)+
      geom_bar(data = PlotData_past,aes(x = time, y = screenings, fill = "Actual"), stat="identity",  width = 0.5) +
      geom_point(data= PlotData_future, aes(x = time, y = fcast, colour = "Model"), shape = 8, size = 2) +
      geom_line(data = PlotData_future, aes(x = time, y = fcast, colour = "Model", group = 1), lty = "dashed") +
      geom_segment(data = PlotData, aes(y = 0, yend = max(PlotData$screenings), x = CurrentMon - 0.5, xend = CurrentMon - 0.5), colour = "grey", lty = "dotted", size = 1) +
      ggtitle (paste(CurrentYear, "screening forecast plot")) +
      xlab("Year") +
      ylab("Number of Screens") +
      theme_bw() +
      theme(text = element_text(family = "Verdana"),
            panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            axis.title.y = element_text(size = 8, colour = "#58595B", margin = margin(t = 10, r = 10, b = 100, l = 10)),
            axis.title.x = element_text(size = 8, colour = "#58595B", margin = margin(t = 7, r = 10, b = 0, l = 10)),
            axis.line = element_line(colour = "#58595B", size = .176),
            axis.ticks = element_line(colour = "#58595B", size = .176),
            axis.ticks.length =unit(1.25, "mm"),
            axis.text  = element_text(colour = "#58595B", size = 7),
            legend.title = element_blank(),
            legend.box = "horizontal",
            legend.text = element_text(colour = "#58595B", size = 7, lineheight = 1),
            legend.direction = "horizontal",
            legend.justification = "bottom",
            plot.title = element_text(size = 10,  face = "bold", colour = "#58595B")
            
      ) +
      scale_y_continuous(label = comma, expand = c(0,0)) +
      scale_colour_manual(name = "" , values = cols) +
      scale_fill_manual(name = "", values = cols)
    
    if ((input$response1 == "State" & input$State1 == "Total_Screeners") || input$response1 == "SAS" ) {
      p <- p + geom_bar(data = PlotData_future, aes(x = time, y = target, colour = "Target"), width = 0.5, size = 0.5, stat = 'identity', fill = "transparent") +
        geom_bar(data = PlotData_past, aes(x = time, y = target, colour = "Target"), width = 0.5, size = 0.5, stat = 'identity', fill = "transparent")}
    
    
    ggplotly()
    
  })
  
  output$plotTS <- renderPlotly({ 
    
    ### reading data   
    
    if (input$response2 %in% c("State")){ 
      TS_Plot_data <- window(StateTS, end = c(CurrentYear,CurrentMon -1)) 
    }
    else if (input$response2 == "SAS"){
      index <- match(input$SAS_code2, SASlist) 
      TS_Plot_data <- window(SASDashboard2MonDataTS[[index]], end = c(CurrentYear,CurrentMon -1))}
    else if (input$response2 == "Age"){
      index <- match(input$AgeGroup2, AgeList) 
      TS_Plot_data <- window(AgeDashboard2MonDataTS[[index]], end = c(CurrentYear,CurrentMon -1))}
    
    
    p <- ggseasonplot(TS_Plot_data[,input$TS2], year.labels=TRUE, year.labels.left=TRUE) +
      xlab ("Time") + 
      ylab ( paste("Number of",  input$TS2 )) +
      ggtitle(paste(input$TS2, "time series plot "))+
      theme_bw() +
      theme(text = element_text(family = "Verdana"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            axis.title.y = element_text(size = 8, colour = "#58595B", margin = margin(t = 0, r = 10, b = 15, l = 10)),
            axis.title.x = element_text(size = 8, colour = "#58595B", margin = margin(t = 7, r = 10, b = 0, l = 10)),
            axis.line = element_line(colour = "#58595B", size = .176),
            axis.ticks = element_line(colour = "#58595B", size = .176),
            axis.ticks.length =unit(1.25, "mm"),
            axis.text  = element_text(colour = "#58595B", size = 7),
            legend.title = element_blank(),
            legend.box = "horizontal",
            legend.text = element_text(colour = "#58595B", size = 7, lineheight = 1),
            legend.direction = "horizontal",
            legend.justification = "bottom",
            plot.title = element_text(size = 10,  face = "bold", colour = "#58595B")
      )
    
    
    ggplotly()
    
    
  })

  
 
}


shinyApp(ui, server)
