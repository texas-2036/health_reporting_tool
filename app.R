## load packages 
library(shiny)
library(shinydashboard)
library(readr)

## load data

## full application

### app ui
ui <- dashboardPage(
  dashboardHeader(title="Health Reporting Tool"),
  dashboardSidebar(),
  dashboardBody()
)

### app server
server <- function(input, output) { 
  
  }

shinyApp(ui, server)