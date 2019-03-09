#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Different modules of our application

library(shiny)
library(shinythemes)
library(shinydashboard)
library(markdown)
library(plotly)

# Image URL 

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = shinytheme("slate"),
                
                navbarPage(title="WORLD BANK",
                           tabPanel(title="TIME SERIE",icon=icon("fas fa-chart-line")),
                           
                           tabPanel(title="WORLD MAP",icon=icon("far fa-globe")),
                                    
                           
                           tabPanel(title="VISUAL DATA ANALYSIS",icon=icon("bar-chart-o")),
                           
                           navbarMenu(title="INFO",icon=icon("far fa-info"),
                                      tabPanel(title="PARTICIPANTS",icon=icon("fas fa-user")),
                                      tabPanel(title="DATASET",icon=icon("fas fa-database")))
                          
                           
                
  )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
  

   

  
}



# Run the application 
shinyApp(ui = ui, server = server)

