library(shiny)
library(dplyr)
library(ggplot2)
library(grid)
library(plotly)
library(DT)
library(tidyverse)
library(reshape2)

source('./app/components/dataElementsTab.R')
source('./app/components/dataElementUsesTab.R')
 

####################################### TEMP ###################################################
dataElements = as_tibble(read_csv('./data/preparedData/dataElements.csv'))
##############################################################################################
 
ui = fluidPage(    
  br(),         
  tabsetPanel(type = "tabs",
              tabPanel("Overview", 
                       fluidRow(
                         column(12,
                                
                                br(),  
                                includeHTML("./app/www/partials/overview.html")
                         ) # end col  
                       ) #end row 
              ), 
              dataElementsTab,
              #dataElementUsesTab,
              tabPanel("Data element uses", 
                       
                       fluidRow(
                         column(12,
                                
                                br(),
                                div("Coming soon")
                                
                         ) # end col  
                
                       ) #end row 
              ), 
              tabPanel("Extended usage notes", 
                       
                       fluidRow(
                         column(12,
                                
                                br(),
                                div("Coming soon")
                                
                         ) # end col  
                         
                       ) #end row 
              ), 
              tabPanel("Coverage and quality metrics", 
                       
                       fluidRow(
                         column(12,
                                
                                br(),
                                div("Coming soon")
                                
                         ) # end col  
                       ) #end row 
              ), 

              tabPanel("Dowload data dictionaries", 
             fluidRow(
               column(12,
                      
                      br(),
                      div('Coming soon')
                      
               ) # end col  
             ) #end row 
    )
     
    )

)
  

server = function(input, output) {
  
  output$dataElements = DT::renderDataTable({
    
    dataElements %>%
      select(-dataElementID)
    
    
  })
  
  output$rulesAndCoverage = DT::renderDataTable({
    return(rulesAndCoverage)
    
  })
  

}

shinyApp(ui, server)




