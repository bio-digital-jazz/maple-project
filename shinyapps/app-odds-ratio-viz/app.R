
################################ UI ########################################

library(shiny)
library(plotly)


# Define UI for app that draws a histogram ----
ui = fluidPage(
  
  # App title ----
  titlePanel("The odds ratio viz-ualiser!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput("Prob1",
                  "Baseline probability:",
                  min = 0,
                  max = 1,
                  value = 0.5),
    #
    sliderInput("OddsRatio",
                "Odds ratio:",
                min = 1,
                max = 5,
                value = 1,
                step = 0.1)
    ),
  
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)


server = function(input, output) {

    
  output$distPlot = renderPlot({
    
    # Prep data
    Prob2 = (input$Prob1 * input$OddsRatio) / (1 - input$Prob1 + input$Prob1 * input$OddsRatio)
    Prob2 = round(Prob2, 2)
    RiskDiff  = round(Prob2 - input$Prob1, 2)
    RiskRatio = Prob2 / input$Prob1
    RiskRatio = round(RiskRatio, 1)
    
    #
    RiskText = paste0("RD = ", RiskDiff, ";   RR = ", RiskRatio, ";   OR = ", input$OddsRatio)
    
    ## render data as plot
    ggplot() +
      geom_col(aes(x=1:2, y=c(input$Prob1, Prob2))) +
      geom_text(aes(x=1:2, y=c(input$Prob1, Prob2), label=c(input$Prob1, Prob2)), position=position_dodge(width=0.9), vjust=-0.25) + 
      geom_text(aes(x=1.5, y=-0.1, label=RiskText)) +
      coord_cartesian(xlim=c(0,3), ylim=c(-0.2,1.2)) + 
      labs(title = "Unpack and understand your odds ratio here!", 
           x="", y="Probability")
    
    ##plot(1:2, c(input$Prob1, Prob2), ylim=c(0,1))
    
  })
  
}


shinyApp(ui = ui, server = server)




