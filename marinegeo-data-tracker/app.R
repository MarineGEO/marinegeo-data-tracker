#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(here)
library(forcats)
library(readr)
library(tidyr)
library(dplyr)
source(here("scripts/functions.R"))

data <- prep(read_csv(here("data/data-status-tracker.csv")))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("MarineGEO Data Tracker"),
  
  conditionalPanel(
      condition = " input.tabs ==  'Site' ",
      selectInput("selected_siteName", "Site", sort(as.vector(sites)))
    ),    
  conditionalPanel(
      condition = " input.tabs ==  'Project' ",
      selectInput("selected_project", "Project", unique(data$Project))
    ),

    tabsetPanel(id="tabs", type="tabs", 
                tabPanel("Site", plotOutput("plotSite", height="600px")),
                tabPanel("Project", plotOutput("plotYear", height="600px")),
                selected = "Site"
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$plotSite <- renderPlot({
      site_code <- names(which(sites==input$selected_siteName))
      trackerSite(data, site_code)
   })  
   
   output$plotYear <- renderPlot({
     trackerProject(data, input$selected_project)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

