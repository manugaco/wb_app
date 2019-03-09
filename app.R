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

# Image URL 

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),
                
                navbarPage(title=div(div(id = "img-id",img(src="wb.png")),"WORLD BANK"),
                           tabPanel("TIME SERIE",icon=icon("fas fa-chart-line")),
                           tabPanel("WORLD MAP",icon=icon("far fa-globe")),
                           tabPanel("SCATTER PLOT",icon=icon("bar-chart-o")),
                           navbarMenu("INFO",icon=icon("far fa-info"),
                                      tabPanel("PARTICIPANTS",icon=icon("fas fa-user")),
                                      tabPanel("DATASET",icon=icon("fas fa-database")))
                )
                
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
}



# Run the application 
shinyApp(ui = ui, server = server)

