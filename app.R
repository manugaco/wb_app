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
library(shiny)
library(tidyverse)
library(WDI)
library(wbstats)
library(progress)
library(Hmisc)
library(maps)
library(viridis)
library(mapproj)

#source("Data.R")

# Image URL 

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = shinytheme("superhero"),
                
                navbarPage(title="WORLD BANK",
                           tabPanel(title="TIME SERIES",icon=icon("fas fa-chart-line"),
                                    titlePanel("Time Series"),
                                    
                                    sidebarPanel(
                                      
                                      # Input: Selector for choosing dataset ----
                                      selectInput(inputId = "variable",
                                                  label = "Choose a variable:",
                                                  choices = names(list)),
                                      
                                      # Input: Numeric entry for number of obs to view ----
                                      selectInput(inputId = "country",
                                                   label = "Choose the country:",
                                                  choices = countries
                                                   )
                                    )
                                    
                                    ,

                                    mainPanel(plotlyOutput("tm"))),
                                    
                           
                           tabPanel(title="WORLD MAP",icon=icon("far fa-globe"),
                                    titlePanel("WORLD MAP"),
                                    
                                    sidebarPanel(
                                      
                                      # Input: Selector for choosing dataset ----
                                      selectInput(inputId = "variable",
                                                  label = "Choose a variable:",
                                                  choices = names(list)),
                                      sliderInput(inputId = "year",
                                                  label = "Select Year:",
                                                  min = 1960,
                                                  max = 2018,
                                                  value = 1990)
                                      
                                    ),
                                      
                     
                                    mainPanel(plotOutput("mp"))),
                                    
                        
                           tabPanel(title="VISUAL DATA ANALYSIS",icon=icon("bar-chart-o")),
                           
                           navbarMenu(title="INFO",icon=icon("far fa-info"),
                                      tabPanel(title="PARTICIPANTS",icon=icon("fas fa-user")),
                                      tabPanel(title="DATASET",icon=icon("fas fa-database")))
                          
  
)
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$tm = renderPlotly({
    
    #This is the variable selection
    
    var <- list[[input$variable]]
    
    #List of countries from wbstats package (in order to have a reference list of countries to match maps and source[var])
    countries_ls <- wbcountries(lang = 'en')
    #Removing NAs
    countries <- countries_ls$country[!is.na(countries_ls$regionID)]
    
    #Selecting those countries that are available in the dataset
    var = var[which(var$country %nin% setdiff(var$country, countries)), ]
    
    #Deleting countries without position information
    
    no_pos <- c("Channel Islands","Eswatini", "Gibraltar", "Hong Kong SAR, China", "Korea, Dem. People’s Rep.",
                "Macao SAR, China", "Tuvalu", "West Bank and Gaza")
    
    for(i in 1:length(no_pos)){
      var <- var[!var$country == no_pos[i], ]
    
      }
    
    #Extracting the countries list from world map package
    map <- map_data('world')
    #map <- map[!duplicated(map$region), ]
    map <- fortify(map)
    
    #There are some country names that does not match, some tidy is necessary
    
    bad <- c("Antigua", "Bahamas", "Virgin Islands", "Brunei", "Cape Verde", 
             "Democratic Republic of the Congo", "Republic of Congo", "Ivory Coast", "Egypt", "Gambia",
             "Iran", "North Korea", "Kyrgyzstan", "Laos", "Macedonia", "Micronesia", "Russia", "Sint Marteen",
             "Slovakia", "Saint Kitts", "Saint Lucia", "Saint Martin", "Saint Vincent", "Syria", "Trinidad",
             "UK","USA", "Venezuela", "Virgin Islands", "Yemen")
    
    good <- sort(setdiff(var$country, map$region))
    
    for(i in 1:length(bad)){
      map <- map %>% mutate(region = if_else(region == bad[i], good[i], region))
    }
    
    df <- left_join(map, var, by = c('region' = 'country'))
    df <- df[!duplicated(df$region), ]
    df$region
    #Selecting the country and formating the input of the plot
    
    rownames(df) <- seq(1, nrow(df), by = 1)
    df2 <- df[which(df$region== input$country),]
    ts <- t(df2[,-(1:14)])
    ts <- cbind(rownames(ts), ts[,1])
    colnames(ts) <- c("year", "region")
    rownames(ts) <- seq(1, nrow(ts), by = 1)
    ts <- data.frame(ts, stringsAsFactors = FALSE)
    ts$region <- round(as.numeric(ts[,2]), 3)
    
    #Plotting the time series
    
    plot_ly(ts, x = ~year, y = ~region, type = 'scatter', mode = 'lines') %>%
      layout(
        title = paste(input$variable, df2$region, sep = " of "), yaxis = list(title = input$variable))
  })
   
}
  
  




# Run the application 
shinyApp(ui = ui, server = server)

