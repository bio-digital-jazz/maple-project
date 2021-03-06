library(shiny)
library(dplyr)
library(DT)
library(readr)
library(httr)
library(jsonlite)



# get data
dataAssetsDF0<- read_csv("./data/preparedData/dataAssets.csv")
usersAndRolesDF0 <- read_csv("./data/preparedData/usersAndRoles.csv")
assetsAndResourcesDF0 <- read_csv("./data/preparedData/assetsAndResources.csv")

ui <- fluidPage(
  

  
  
  
  fluidRow(
    column(3,
          ""  
    ),
    
    column(9,
           selectInput("assetChoice", 
                       "", 
                       width='700px',
                       choices = c("Choose a data asset", as.list(sort(dataAssetsDF0$Name))), 
                       selected = NULL)
          
    ),
    hr()
  ),
  
  fluidRow(
    column(2,
           ""),
    column(8,
           uiOutput("allAssets"))
  )
  
)



server <- function(input, output) {
  
  # TABS
  output$allAssets = renderUI({
    if (input$assetChoice != "Choose a data asset") {

      div(
        textOutput("assetDetailDescription"),
        br(),
        tabsetPanel(type = "tabs",
                    tabPanel("Overview", 
                             br(),
                             tableOutput("assetDetailOverview")),
                    tabPanel("Users and roles", 
                             br(),
                             DTOutput("assetUsersAndRoles")),
                    tabPanel("Supporting documentation", 
                             br(),
                             DTOutput("assetDetailDocumentation"))
        )
        
      )
    } else {
      includeHTML("./overPartial.html")
    
      }
  })
  
  
  ## ASSET DETAIL OVERVIEW
  output$assetDetailOverview = renderTable({
    if (input$assetChoice != "Choose a data asset") {
      
      assetDetails <- dataAssetsDF0 %>%
        filter(Name == input$assetChoice) %>%
        select(-dataAssetID, -"Usage notes available", -Description)
      
      df <- as.data.frame(t(assetDetails))
      df$Description <- colnames(assetDetails)
      colnames(df) = c("Data", "Description")
    
      return(df[2:1])
      
 
      
      return(t(assetDetails))
    
      
     
    }
    
  })
  
  
  ## USERS AND ROLES
  output$assetUsersAndRoles= renderDT({
    if (input$assetChoice != "Choose a data asset") {
      
      
      citibike <- fromJSON("http://localhost:8000/api/data-assets/v1/breach/")
      
      return(data.frame(citibike))
      
      
      # assetDetails <- dataAssetsDF0 %>%
      #   filter(Name == input$assetChoice)
      # 
      # usersAndRoles <- usersAndRolesDF0 %>%
      #   filter(usersAndRolesDF0$dataAssetID ==  assetDetails$dataAssetID) %>%
      #   select(-dataAssetID)
      # 
      # return(usersAndRoles)
     
    }
    
  })
  
  
  
  
  ## ASSET DETAIL DESCRIPTION
  output$assetDetailDescription = renderText({
    if (input$assetChoice != "Choose a data asset") {
      choice <- dataAssetsDF0 %>% 
        filter(Name == input$assetChoice)
      return(paste0("", choice$Description))
      
    } 
  })
  
  ## ASSET DETAIL DOCUMENTATION
  output$assetDetailDocumentation = renderDT({
    if (input$assetChoice != "Choose a data asset") {
      
      
      assetDetails <- dataAssetsDF0 %>%
        filter(Name == input$assetChoice)
      
      assetsAndResources <- assetsAndResourcesDF0 %>%
        filter(assetsAndResourcesDF0$dataAssetID ==  assetDetails$dataAssetID) %>%
        select(-dataAssetID)
      
      return(assetsAndResources)
      
    }
    
  })
  


}


shinyApp(ui = ui, server = server)

