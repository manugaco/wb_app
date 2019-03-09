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
  
  theme = shinytheme("slate"),
                
                navbarPage(title="WORLD BANK",
                           tabPanel(title="TIME SERIE",icon=icon("fas fa-chart-line"),
                                    plotlyOutput("tm")),
                           
                           tabPanel(title="WORLD MAP",icon=icon("far fa-globe"),
                                    plotOutput("mp")),
                        
                        
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
    
    vs <- indicators_name[12]
    var <- list[[vs]]
    
    #Select the country
    
    count <- seq(1, 217, by = 1)
    cnt <- count[sample(1:217, 1)]
    
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
    df2 <- df[which(rownames(df) == cnt),]
    ts <- t(df2[,-(1:14)])
    ts <- cbind(rownames(ts), ts[,1])
    colnames(ts) <- c("year", "region")
    rownames(ts) <- seq(1, nrow(ts), by = 1)
    ts <- data.frame(ts, stringsAsFactors = FALSE)
    ts$region <- round(as.numeric(ts[,2]), 3)
    
    #Plotting the time series
    
    plot_ly(ts, x = ~year, y = ~region, type = 'scatter', mode = 'lines') %>%
      layout(
        title = paste(vs, df2$region, sep = " of "), yaxis = list(title = vs))
  })
   

  output$mp = renderPlot({
    
    #This is the variable selection
    vs <- indicators_name[10]
    
    #This is the year selected
    year <- seq(1960, 2018, by = 1)
    yr <- year[55]
    
    var <- list[[vs]]
    
    countries_ls <- wbcountries(lang = 'en')
    #Removing NAs
    countries <- countries_ls$country[!is.na(countries_ls$regionID)]
    
    #Selecting those countries that are available in the dataset
    var = var[which(var$country %nin% setdiff(var$country, countries)), ]
    
    #Deleting countries without position information
    
    no_pos <- c("Channle Islands","Eswatini", "Gibraltar", "Hong Kong SAR, China", "Korea, Dem. People’s Rep.",
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
             "UK", "Venezuela", "Virgin Islands", "Yemen", "USA")
    
    good <- sort(setdiff(var$country, map$region))
    
    for(i in 1:length(bad)){
      map <- map %>% mutate(region = if_else(region == bad[i], good[i], region))
    }
    
    df <- left_join(map, var, by = c('region' = 'country'))
    df <- df[!duplicated(df$region), ]
    
    #Removing extreme values
    
    if(vs == 1){
      df <- df[-(which(df[,"yr"] > 70000)),]
    }
    
    #Plotting the variablae in the map
    
    ggplot() +
      geom_map(data = df, map = df,
               aes(x=long, y=lat, group = group, map_id = region),
               fill="white", colour="black", size=0.5) +
      geom_map(data = df, map = map, aes(fill=yr, map_id = region),
               colour="black", size=0.5) + 
      scale_fill_viridis(option = 'plasma', guide = "colorbar") +
      coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
      theme_bw() + 
      theme(panel.border = element_blank()) + 
      ggtitle(indicators_name[vs])
    
    
  })
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)

