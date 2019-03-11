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
library(plotly)
library(mapdata)
library(ggthemes)
library(viridisLite)
library(viridis)

# source("Data.R")

# Image URL 

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = shinytheme("superhero"),
                
                navbarPage(title="WORLD BANK DATA",
                           tabPanel(title="TIME SERIES",icon=icon("fas fa-chart-line"),
                                    titlePanel("Time Series"),
                                    
                                    sidebarPanel(
                                      
                                      # Input: Selector for choosing dataset ----
                                      selectInput(inputId = "variable_ts",
                                                  label = "Choose a variable:",
                                                  choices = names(list)),
                                      
                                      # Input: Numeric entry for number of obs to view ----
                                      selectInput(inputId = "country_ts",
                                                   label = "Choose the country:",
                                                  choices = countries
                                                   )
                                    ),

                                    mainPanel(plotlyOutput("tm")))
                           
  ,
                                    
                           
                           tabPanel(title="WORLD HEAT MAP",icon=icon("far fa-globe"),
                                      titlePanel("FUCK MANU"),
                                    
                                    sidebarPanel(
                                      
                                      # Input: Selector for choosing dataset ----
                                      selectInput(inputId = "variable_mp",
                                                  label = "Choose a variable:",
                                                  choices = names(list)),
                                      sliderInput(inputId = "year_mp",
                                                  label = "Select Year:",
                                                  min = 1960,
                                                  max = 2018,
                                                  value = 1990)
                                      
                                    ),
                                      
                     
                                    mainPanel(plotOutput("mp")))
  ,
                                    
                        
                           tabPanel(title="VISUAL DATA ANALYSIS",icon=icon("bar-chart-o"),
                           titlePanel("Visual Data Analysis"),
                                    
                           sidebarPanel(
                                      
                                      # Input: Selector for choosing dataset ----
                                      selectInput(inputId = "variable_1",
                                                  label = "Choose variable for y-axis:",
                                                  choices = names(list)),
                                      selectInput(inputId = "variable_2",
                                                  label = "Choose variable for x-axis:",
                                                  choices = names(list)),
                                      sliderInput(inputId = "year_vda",
                                                  label = "Select Year:",
                                                  min = 1960,
                                                  max = 2018,
                                                  value = 1990)
                                                  
                                      ),
                                    
                                    mainPanel(plotOutput("vda")))
  ,
                           
                           navbarMenu(title="INFO",icon=icon("far fa-info"),
                                      tabPanel(title="PARTICIPANTS",icon=icon("fas fa-user")),
                                      
                                      tabPanel(title="DATASET",icon=icon("fas fa-database"))),
                           
                                      tabPanel(title="SETTINGS" , icon=icon("fas fa-cogs"))
                          
  
))


server <- function(input, output) {
  
  output$tm = renderPlotly({
    
    #This is the variable selection
    
    var_ts <- list[[input$variable_ts]]
    
    #Selecting those countries that are available in the dataset
    var_ts = var_ts[which(var_ts$country %nin% setdiff(var_ts$country, countries)), ]
    
    #Selecting those countries that are available in the dataset
    var = var[which(var$country %nin% setdiff(var$country, countries)), ]
    #Deleting countries without position information
    
    no_pos <- c("Channel Islands","Eswatini", "Gibraltar", "Hong Kong SAR, China", "Korea, Dem. People’s Rep.",
                "Macao SAR, China", "Tuvalu", "West Bank and Gaza")
    
    for(i in 1:length(no_pos)){
      var_ts <- var_ts[!var_ts$country == no_pos[i], ]
    
    }
    
    #Selecting the country and formating the input of the plot
    
    rownames(var_ts) <- seq(1, nrow(var_ts), by = 1)
    df2 <- var_ts[which(var_ts$country == input$country_ts),]
    ts <- t(df2[,-(1:9)])
    ts <- cbind(rownames(ts), ts[,1])
    colnames(ts) <- c("year", "region")
    rownames(ts) <- seq(1, nrow(ts), by = 1)
    ts <- data.frame(ts, stringsAsFactors = FALSE)
    ts$region <- round(as.numeric(ts[,2]), 3)
    
    #Plotting the time series
    
    plot_ly(ts, x = ~year, y = ~region, type = 'scatter', mode = 'lines', line = list(color = 'blue')) %>%
      layout(title = input$country,
             yaxis = list(title = input$variable_ts, color = 'white'),
             xaxis = list(title = 'year', tickangle = 45, color = 'white'),
             plot_bgcolor='rgb(150,150,150)', 
             paper_bgcolor = 'rgb(100,100,100)')
  })
  
  output$mp = renderPlot({
    
   
    
    #This is the year selected
    vs = input$variable_mp
    yr_mp <- input$year_mp
    
    var_mp <- list[[input$variable_mp]]
    
    #Selecting those countries that are available in the dataset
    var_mp = var_mp[which(var_mp$country %nin% setdiff(var_mp$country, countries)), ]
    
    #Deleting countries without position information
    
    no_pos <- c("Channel Islands","Eswatini", "Gibraltar", "Hong Kong SAR, China", "Korea, Dem. People’s Rep.",
                "Macao SAR, China", "Tuvalu", "West Bank and Gaza")
    
    for(i in 1:length(no_pos)){
      var_mp <- var_mp[!var_mp$country == no_pos[i], ]
    }
    
    #Extracting the countries list from world map package
    map_mp <- map_data('world')
    #map <- map[!duplicated(map$region), ]
    map_mp <- fortify(map_mp)
    
    #There are some country names that does not match, some tidy is necessary
    
    bad <- c("Antigua", "Bahamas", "Virgin Islands", "Brunei", "Cape Verde", 
             "Democratic Republic of the Congo", "Republic of Congo", "Ivory Coast", "Egypt", "Gambia",
             "Iran", "North Korea", "Kyrgyzstan", "Laos", "Macedonia", "Micronesia", "Russia", "Sint Marteen",
             "Slovakia", "Saint Kitts", "Saint Lucia", "Saint Martin", "Saint Vincent", "Syria", "Trinidad",
             "UK", "USA", "Venezuela", "Virgin Islands", "Yemen")
    
    good <- sort(setdiff(var_mp$country, map_mp$region))
    
    for(i in 1:length(bad)){
      map_mp <- map_mp %>% mutate(region = if_else(region == bad[i], good[i], region))
    }
    
    df_mp <- left_join(map_mp, var_mp, by = c('region' = 'country'))
    df_mp <- df_mp[!duplicated(df_mp$region), ]
    
    
    #Plotting the variablae in the map
    
    fill_mp <- df_mp[, (names(df_mp) %in% yr_mp)]
    
    ggplot() +
      geom_map(data = map_mp, map = map_mp,
               aes(x = long, y = lat, group = group, map_id = region),
               fill="white", colour="black", size=0.5) +
      geom_map(data = df_mp, map = map_mp, aes(fill = fill_mp, map_id = region),
               colour="black", size = 0.5) + labs(fill="") + ggtitle(input$variable_mp) +
      scale_fill_gradientn(colours = c('#461863','#404E88','#2A8A8C','#7FD157','red4')
                           ,values = scales::rescale(c(100,96581,822675,3190373,10000000))
      ) + xlab("") + ylab("") +
      theme(text = element_text(family = 'Gill Sans', color = 'white')
            ,plot.title = element_text(size = 20)
            ,plot.subtitle = element_text(size = 14)
            ,axis.ticks = element_blank()
            ,axis.text = element_blank()
            ,panel.grid = element_blank()
            ,panel.background = element_rect(fill = 'grey30')
            ,plot.background = element_rect(fill = 'grey30')
            ,legend.position = c(.18,.36)
            ,legend.background = element_blank()
            ,legend.key = element_blank()
      ) +
      annotate(geom = 'text'
               ,label = 'Source : World Bank Data'
               ,x = 18, y = -55
               ,size = 3
               ,family = 'Gill Sans'
               ,color = 'white'
               ,hjust = 'left'
      )
    
  })
  
  output$vda = renderPlot({
    
    #Variables
    
    var1 <- list[[input$variable_1]]
    var2 <- list[[input$variable_2]]
    
    #Years
    
    yr_vda <- input$year_vda
    
    #Merging the data in one dataset

    var1 <- var1[which(var1$country %nin% setdiff(var1$country, countries)), ]
    var1 <- var1 %>% select(country, income, region, which(colnames(var1) == yr_vda))
    var2 <- var2[which(var2$country %nin% setdiff(var2$country, countries)), ]
    var2 <- var2 %>% select(country, income, region, which(colnames(var2) == yr_vda))
    
    df_vda <- merge(var1, var2, by = "country")
    df_vda <- df_vda[, -(5:6)]
    colnames(df_vda) <- c("country","income","region", "x", "y")
    
    labels <- sample(df_vda$country, 30)
    
    ggplot(data = df_vda, aes(x=log(x), y=log(y), color = income)) +
      geom_point() + geom_text(aes(label = country), check_overlap = TRUE, vjust = 1, hjust = 1, color = "white") +
      xlab(input$variable_2) + ylab(input$variable_1) + 
      scale_colour_discrete(name = "income", 
                            breaks = levels(df_vda$region),
                            labels = levels(df_vda$region)) +
      theme(text = element_text(family = 'Gill Sans', color = 'white')
            ,plot.title = element_text(size = 20)
            ,axis.text = element_text(color = 'white')
            ,panel.background = element_rect(fill = 'grey30')
            ,plot.background = element_rect(fill = 'grey30')
            ,legend.background = element_blank()
            ,legend.key = element_blank())
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

#Uploading application
#rsconnect::setAccountInfo(name='manugaco',
#                          token='7B67C1050D3D1BA3206C78B5F9DEC18F',
#                          secret='A+LV652hIeKPi/BrNaI9hk0CQ/L2NDL+q0GE6jl9')
#rsconnect::deployApp()
