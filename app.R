
#

#ATTENTION - Run first Data.R to load the dataset and set the envoirement objects

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Different modules of our application

library(DT)
library(shiny)
library(rsconnect)
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
library(gridExtra)
library(vembedr)
library(htmltools)

#Access to data, it is marked to upload it to the shiny server, if you run the app in local, unmark
#source("Data.R")


#Or run the script Data.R once, to store the data in local.
#We have done it because it is more efficient to store all the dataset after run the app,
#otherwise it takes much time getting the data (there is one dataset each variable)

#This part is to load the data in shiny server, do not use in local (leave commented load("env.Rdata"))

load("env.Rdata")

# Image wb URL 
t <- tags$a(href= "https://data.worldbank.org" , tags$img(src="bwlogo.png", heigth = 25, width = 25))
tgit <- tags$a("GitHub",href="https://github.com/manugaco/Data_Tidying", tags$img(src="github.png",height=20,width=20))
uni <- tags$a(href= "https://www.uc3m.es/Inicio" , tags$img(src="uc3m.png", heigth = 25, width = 25))

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("superhero"),
  navbarPage(t,
             tabPanel(title="MAIN WINDOW",icon=icon("fas fa-home"),
                      sidebarPanel(
                        h3("Testing World Bank API"),
                        helpText(" Select a variable, a country and a year, then, the API will call to the World bank indicator database to download the desired information."),
                        selectInput(inputId = "searchcountry",
                                    label = "Choose country:",
                                    choices = countries,
                                    selected = "Spain"),
                        selectInput(inputId = "searchvariable",
                                    label = "Choose variable:",
                                    choices = names(list)),
                        sliderInput("searchyear", label = "Year", min = 1960, 
                                    max = 2018, value = 1960),
                        
                        br(),
                        textOutput("data")
                        
                        
                      )  ,
                      
                      
                      mainPanel(
                        img(src="wb.png",align="center",height="133",weight="570")%>%div(align="center"),
                        
                        br(),
                        br(),
                        p("The purpose of this project is to perform a Shiny app using data from the Word Bank with the aim of creating interactive and heat maps about economic and demographic data."),
                        h2("World Bank API",align="center"),
                        
                        p("The World Bank is a source of financial and technical assistance to developing countries 
                          around the world. The World Bank is not a bank in the ordinary sense but a unique partnership to reduce poverty 
                          and support development. The World Bank Group comprises five institutions managed by their member 
                          countries"),
                        embed_url("https://www.youtube.com/watch?v=gKpTL8KVy1Q&t=214s")%>%div(align = "center"),
                        br(),
                        
                        h2("Source of Data",align="center"),
                        p(" The used dataset has been downloaded from the world bank data base by means of the  world bank indicators API."),
                        p("This dataset is made up with information about topics such as Ecomomic growth, Demographics and education, in particular:"), 
                        p("* Economy and Growth: GDPpc, Inflation, Trade Balance, Goverment debt, consumption and expenses."),
                        p("* Demography: Total population, active population, unemployment rate, life expectancy, birth rate and death rate."),
                        p("* Education: Education expenses, compulsory education, literacy rate, secondary transitions, trained teachers and repeaters."),
                        p("* Trade: Goods trade, tools duties, import terms, international tourism, logistic performance and taxes."),
                        p("* Science and tech: High-tech exports, R&D expenses, papers published, Researchers, patents request and intellectual property costs."),
                        p("For more information about the API, visit the ",
                          a("World Development Indicators.", 
                            href = "https://datahelpdesk.worldbank.org")),
                        br(),
                        
                        h2("App Content",align="center"),
                        
                        p(strong("- Time series "),"of the selected variable and country, being an interactive chart."),
                        p(strong("- World heat map ")," representing the selected variable, interactive on time."),
                        p(strong("- Animated plot "),"Relevant information such as population, unemployment rate, and so on of a certain country."),
                        p(strong("- Exploratory Analysis "),"Descriptive information about the variables (density functions, histograms and boxplots), grouped by region or income.")
                        )
                      )
             
             
             
             ,tabPanel(title="TIME SERIES",icon=icon("fas fa-chart-line"),
                       titlePanel("Time Series"),
                       
                       sidebarPanel(
                         
                         # Input: Selector for choosing dataset ----
                         selectInput(inputId = "variable_ts",
                                     label = "Choose variable:",
                                     choices = names(list)),
                         
                         # Input: Numeric entry for number of obs to view ----
                         selectInput(inputId = "country_ts1",
                                     label = "Choose country:",
                                     choices = countries,
                                     selected = "Spain"),
                         selectInput(inputId = "country_ts2",
                                     label = "Choose country:",
                                     choices = countries,
                                     selected = "France"),
                         selectInput(inputId = "country_ts3",
                                     label = "Choose country:",
                                     choices = countries,
                                     selected = "United Kingdom")
                       ),
                       
                       mainPanel(plotlyOutput("tm")))
             
             ,
             
             
             tabPanel(title="WORLD HEAT MAP",icon=icon("far fa-globe"),
                      titlePanel("World Heat Map"),
                      
                      sidebarPanel(
                        
                        # Input: Selector for choosing dataset ----
                        selectInput(inputId = "variable_mp",
                                    label = "Choose variable:",
                                    choices = names(list)),
                        sliderInput(inputId = "year_mp",
                                    label = "Select Year:",
                                    min = 1960,
                                    max = 2018,
                                    value = 2008)
                        
                      ),
                      
                      
                      mainPanel(plotOutput("mp")))
             ,
             
             
             tabPanel(title="ANIMATED CHART",icon=icon("bar-chart-o"),
                      titlePanel("Bivariate Data Visualization"),
                      
                      sidebarPanel(
                        
                        # Input: Selector for choosing dataset ----
                        selectInput(inputId = "variable_1",
                                    label = "Choose variable for y-axis:",
                                    choices = names(list),
                                    selected = "Life expectancy at birth, total (years)"),
                        selectInput(inputId = "variable_2",
                                    label = "Choose variable for x-axis:",
                                    choices = names(list),
                                    selected = "GDP per capita (current US$)"),
                        h6("Select log transformation:"),
                        checkboxInput("log_x","Log scale x-axis", value = TRUE),
                        checkboxInput("log_y","Log scale y-axis", value = TRUE)),
                      
                      mainPanel(plotlyOutput("vda")))
             ,
             
             tabPanel(title="EXPLORATORY ANALYSIS",icon=icon("bar-chart-o"),
                      titlePanel("Univariate Data Visualization"),
                      
                      sidebarPanel(
                        
                        # Input: Selector for choosing dataset ----
                        selectInput(inputId = "variable_uda",
                                    label = "Choose variable:",
                                    choices = names(list),
                                    selected = "GDP per capita (current US$)"),
                        selectInput(inputId = "group_uda",
                                    label = "Group by:",
                                    choices = c("Income", "Region", "None"),
                                    selected = "None"),
                        selectInput(inputId = "plotype",
                                    label = "Select plot type:",
                                    choices = c("Boxplot", "Histogram", "Density"), 
                                    selected = "Density"),
                        sliderInput(inputId = "year_uda",
                                    label = "Select Year:",
                                    min = 1960,
                                    max = 2018,
                                    value = 2008),
                        h6("Select log transformation:"),
                        checkboxInput("log_uda","Log scale", value = TRUE)),
                      
                      mainPanel(plotOutput("uda")))
             ,
             navbarMenu(title="INFO",icon=icon("far fa-info"),
                        tabPanel(tgit),
                        tabPanel(title = "REFERENCES",icon=icon("fas fa-question-circle"),
                                 sidebarPanel(  h1("References"),
                                   p("Shinny app guide:",a( "https://shiny.rstudio.com/gallery/", href="https://shiny.rstudio.com/gallery/")),
                                   p("World Bank API :",a( "https://datahelpdesk.worldbank.org", href="https://datahelpdesk.worldbank.org")),
                                   p("Plotly Shinny examples:",a( "https://plot.ly/r/shiny-tutorial/", href="https://plot.ly/r/shiny-tutorial/")),
                                   p("World Bank Page:",a( "https://www.worldbank.org/", href="https://www.worldbank.org/"))
                                 )
                               
                                 
                                 ),
                        tabPanel(title="GROUP MEMBERS",icon=icon("fas fa-users"),
                                 
                                 h1("GROUP MEMBERS"),
                                 
                                 img(src="0.jpeg",align="center",height="133",weight="570")%>%div(align="center"),
                                 br(),
                                 h3("Manuel García Corbí")%>%div(align="center"),
                                 p("Linkedin: ", a("https://www.linkedin.com/in/manugaco/",href="https://www.linkedin.com/in/manugaco/"))%>%div(align="center"),
                                 br(),
                                 img(src="1.jpeg",align="center",height="133",weight="570")%>%div(align="center"),
                                 br(),
                                 h3("Antonio Polo de Alvarado")%>%div(align="center"),
                                 p("Linkedin: ", a("https://www.linkedin.com/in/antoniopolodealvarado/",href="https://www.linkedin.com/in/antoniopolodealvarado/"))%>%div(align="center")
                                 
                                 
                                 
                                 )
                        
                        
             ),
             
             tabPanel(title="SETTINGS" , icon=icon("fas fa-cogs")
                      ,mainPanel(shinythemes::themeSelector()))
             
             ))

#Defining the server function
server<-function(input,output){
  
  output$data = renderText({
    var=list[[input$searchvariable]]
    varyear = input$searchyear
    varcountry = input$searchcountry
    index=which(var$country==varcountry)
    
    t=var[index,as.character(varyear)]
    print(paste0("The value of the desired variable is: ", t))
    
  })
  
  output$tm = renderPlotly({
    
    #This is the variable selection
    
    var_ts <- list[[input$variable_ts]]
    
    #Selecting those countries that are available in the dataset
    var_ts <- var_ts[which(var_ts$country %nin% setdiff(var_ts$country, countries)), ]
    
    #Selecting those countries that are available in the dataset
    var <- var[which(var$country %nin% setdiff(var$country, countries)), ]
    
    #Deleting countries without position information
    no_pos <- c("Channel Islands","Eswatini", "Gibraltar", "Hong Kong SAR, China", "Korea, Dem. People’s Rep.",
                "Macao SAR, China", "Tuvalu", "West Bank and Gaza")
    
    for(i in 1:length(no_pos)){
      var_ts <- var_ts[!var_ts$country == no_pos[i],]
    }
    
    #Selecting the country and formating the input of the plot
    c1 <- input$country_ts1
    c2 <- input$country_ts2
    c3 <- input$country_ts3
    
    cnt <- c(c1, c2, c3)
    rownames(var_ts) <- seq(1, nrow(var_ts), by = 1)
    df2 <- var_ts[which(var_ts$country %in% cnt),]
    ts <- t(df2[,-(1:9)])
    ts <- cbind(rownames(ts), ts[,1:3])
    colnames(ts) <- c("year", "region1", "region2", "region3")
    rownames(ts) <- seq(1, nrow(ts), by = 1)
    ts <- data.frame(ts, stringsAsFactors = FALSE)
    cols <- c("region1", "region2", "region3")
    ts[cols] <- sapply(ts[cols], as.numeric)
    
    #Plotting the time series using plotly dynamic chart
    
    plot_ly(ts, x = ~year, y = ~region1, name = input$country_ts1,
            type = 'scatter', mode = 'lines+markers', line = list(color = 'blue')) %>%
      add_trace(y = ~region2, name = input$country_ts2, mode = 'lines+markers', line = list(color = 'red')) %>%
      add_trace(y = ~region3, name = input$country_ts3, mode = 'lines+markers', line = list(color = 'green')) %>%
      layout(title = input$variable_ts,
             font = list(color = 'white'),
             yaxis = list(title = input$variable_ts, color = 'white'),
             xaxis = list(title = 'year', tickangle = 45, color = 'white',
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 mo",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 mo",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 yr",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "YTD",
                                step = "year",
                                stepmode = "todate"),
                              list(step = "all"))),
                          rangeslider = list(type = "year")
             ),
             plot_bgcolor='rgb(150,150,150)', #matching color with the shiny theme (superhero) which is default set.
             paper_bgcolor = 'rgb(100,100,100)')
    
    
  })
  
  output$mp = renderPlot({
    
    #This is the year selected
    
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
    
    #Deleting those countries do not match
    
    for(i in 1:length(bad)){
      map_mp <- map_mp %>% mutate(region = if_else(region == bad[i], good[i], region))
    }
    
    #Mergin the datasets and deleting duplicated entries
    
    df_mp <- left_join(map_mp, var_mp, by = c('region' = 'country'))
    df_mp <- df_mp[!duplicated(df_mp$region), ]
    
    #Creating the filling variable, as it is necessary as input for the heat map
    
    fill_mp <- df_mp[, (names(df_mp) %in% yr_mp)]
    
    #Plotting the variablas in the map with ggplot
    
    ggplot() +
      geom_map(data = map_mp, map = map_mp,
               aes(x = long, y = lat, group = group, map_id = region),
               fill="white", colour="black", size=0.5) +
      geom_map(data = df_mp, map = map_mp, aes(fill = fill_mp, map_id = region),
               colour="black", size = 0.5) + labs(fill="") + 
      ggtitle(paste(input$variable_mp, input$year_mp, sep  = " in the year ")) +
      scale_fill_gradientn(colours = c('#461863','#404E88','#2A8A8C','#7FD157','red4')
                           ,values = scales::rescale(c(100,96581,822675,3190373,10000000))
      ) + xlab("") + ylab("") + #These are all arguments for the theme (aim of make it visible)
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
               ,label = 'Source : World Bank'
               ,x = 18, y = -55
               ,size = 3
               ,family = 'Gill Sans'
               ,color = 'white'
               ,hjust = 'left'
      )
  })
  
  output$vda = renderPlotly({
    
    #Variables
    
    var1 <- list[[input$variable_1]]
    var2 <- list[[input$variable_2]]
    
    #Year range
    
    years <- seq(1960, 2018, by = 1)
    
    #Merging the data in one dataset, some wrangling needed
    
    var1 <- var1[which(var1$country %nin% setdiff(var1$country, countries)), ]
    var1.1 <- var1 %>% 
      select(country, income, region)
    var1.2 <- var1[,(colnames(var1) %in% years)]
    var1 <- cbind(var1.1, var1.2)
    var1 <- var1 %>% gather(key = "year", value = v1, which(colnames(var1) %in% years), na.rm = FALSE)
    
    var2 <- var2[which(var2$country %nin% setdiff(var2$country, countries)), ]
    var2.1 <- var2 %>% 
      select(country, income, region)
    var2.2 <- var2[,(colnames(var2) %in% years)]
    var2 <- cbind(var2.1, var2.2)
    var2 <- var2 %>% gather(key = "year", value = v2, which(colnames(var2) %in% years), na.rm = FALSE)
    
    df_vda <- merge(var1, var2, by = c("year", "country"))
    df_vda <- df_vda %>% select(country, income.y, region.y, year, v1, v2)
    colnames(df_vda) <- c("country","income","region", "year", "y", "x")
    
    #Condition for logarithm scale
    
    if(input$log_x){
      df_vda$x = log(df_vda$x)
    }
    
    if(input$log_y){
      df_vda$y = log(df_vda$y)
    }
    
    hide_guides(plot_ly(df_vda, x = ~x, y = ~y, text = ~country, frame = ~year, hoverinfo = "text",
                        type = 'scatter', mode = 'markers',
                        size = ~x, color = ~x, colors = c('#461863','#404E88','#2A8A8C','#7FD157','red4')) %>%
                  layout(title = paste(input$variable_1, input$variable_2, sep = " vs "),
                         font = list(color = 'white'),
                         yaxis = list(title = input$variable_1, color = 'white'),
                         xaxis = list(title = input$variable_2, tickangle = 45, color = 'white'),
                         plot_bgcolor='rgb(150,150,150)', 
                         paper_bgcolor = 'rgb(100,100,100)', hide_legend = T))
  })
  
  output$uda = renderPlot({
    
    #Variables
    
    var_uda <- list[[input$variable_uda]]
    
    #Years
    
    yr_uda <- input$year_uda
    
    #Merging the data in one dataset
    
    var_uda <- var_uda[which(var_uda$country %nin% setdiff(var_uda$country, countries)), ]
    df_uda <- var_uda %>% select(country, income, region, which(colnames(var_uda) == yr_uda))
    
    df_uda <- df_uda[, -(5:6)]
    colnames(df_uda) <- c("country","income","region", "var")
    
    #Condition for logarithm scale
    if(input$log_uda){
      df_uda$var = log(df_uda$var)
    }
    #Histogram all
    p1 <- ggplot(data = df_uda, aes(var)) +
      geom_histogram(color = 'black', fill = "green3") + xlab(input$variable_uda) + ylab("Counts") +
      ggtitle(paste("Histogram", paste(input$variable_uda, input$year_uda, sep = " in the year "), sep = " of the ")) +
      theme(text = element_text(family = 'Gill Sans', color = 'white')
            ,plot.title = element_text(size = 20, color = "white")
            ,axis.text = element_text(color = 'white')
            ,panel.grid = element_blank()
            ,panel.background = element_rect(fill = 'grey30')
            ,plot.background = element_rect(fill = 'grey30')
            ,axis.line = element_line(color = 'white')
            ,legend.background = element_blank()
            ,legend.key = element_blank())
    #Boxplot all
    p2 <- ggplot(data = df_uda, aes(y = var)) +
      geom_boxplot(outlier.colour = 'white', color = 'black', fill = "green3") +
      ggtitle(paste("Boxplot", paste(input$variable_uda, input$year_uda, sep = " in the year "), sep = " of the ")) +
      ylab(input$variable_uda) +
      coord_flip() + 
      theme(text = element_text(family = 'Gill Sans', color = 'white')
            ,plot.title = element_text(size = 20)
            ,axis.text = element_text(color = 'white')
            ,panel.grid = element_blank()
            ,panel.background = element_rect(fill = 'grey30')
            ,plot.background = element_rect(fill = 'grey30')
            ,axis.line = element_line(color = 'white')
            ,legend.background = element_blank()
            ,legend.key = element_blank())
    #Density plot all
    p3 <- ggplot(data = df_uda, aes(var)) +
      geom_density(color = 'black', alpha = 0.3, fill = "green3") +
      xlab(input$variable_uda) + ylab("Density") +
      ggtitle(paste("Density", paste(input$variable_uda, input$year_uda, sep = " in the year "), sep = " of the ")) +
      theme(text = element_text(family = 'Gill Sans', color = 'white')
            ,plot.title = element_text(size = 20)
            ,axis.text = element_text(color = 'white')
            ,panel.grid = element_blank()
            ,panel.background = element_rect(fill = 'grey30')
            ,plot.background = element_rect(fill = 'grey30')
            ,axis.line = element_line(color = 'white')
            ,legend.background = element_blank()
            ,legend.key = element_blank())
    #Histogram for income group
    p1_inc <- ggplot(data = df_uda, aes(var, fill = income)) +
      geom_histogram(color = 'black') +
      ylab(input$variable_uda) + xlab("Counts") +
      ggtitle(paste("Income group histograms", paste(input$variable_uda, input$year_uda, sep = " in the year "), sep = " of the ")) +
      theme(text = element_text(family = 'Gill Sans', color = 'white')
            ,plot.title = element_text(size = 20)
            ,axis.text = element_text(color = 'white')
            ,legend.position= "bottom"
            ,panel.grid = element_blank()
            ,panel.background = element_rect(fill = 'grey30')
            ,plot.background = element_rect(fill = 'grey30')
            ,axis.line = element_line(color = 'white')
            ,legend.background = element_blank()
            ,legend.key = element_blank())
    #Boxplot for income group
    p2_inc <- ggplot(data = df_uda, aes(x = income, y = var, fill = income)) +
      geom_boxplot(outlier.colour = 'white', color = 'black' ) +
      ggtitle(paste("Income group boxplots", paste(input$variable_uda, input$year_uda, sep = " in the year "), sep = " of the ")) +
      ylab(input$variable_uda) +
      theme(text = element_text(family = 'Gill Sans', color = 'white')
            ,plot.title = element_text(size = 20)
            ,axis.text = element_text(color = 'white')
            ,legend.position= "bottom"
            ,panel.grid = element_blank()
            ,panel.background = element_rect(fill = 'grey30')
            ,plot.background = element_rect(fill = 'grey30')
            ,axis.line = element_line(color = 'white')
            ,legend.background = element_blank()
            ,legend.key = element_blank())
    #Density for income group
    p3_inc <- ggplot(data = df_uda, aes(var, fill = income)) +
      geom_density(color = 'black', alpha = 0.3) +
      xlab(input$variable_uda) + ylab("Density") +
      ggtitle(paste("Income group densities", paste(input$variable_uda, input$year_uda, sep = " in the year "), sep = " of the ")) +
      theme(text = element_text(family = 'Gill Sans', color = 'white')
            ,plot.title = element_text(size = 20)
            ,axis.text = element_text(color = 'white')
            ,panel.grid = element_blank()
            ,legend.position= "bottom"
            ,panel.background = element_rect(fill = 'grey30')
            ,plot.background = element_rect(fill = 'grey30')
            ,axis.line = element_line(color = 'white')
            ,legend.background = element_blank()
            ,legend.key = element_blank())
    #Histogram for region
    p1_reg <- ggplot(data = df_uda, aes(var, fill = region)) +
      geom_histogram(color = 'black') +
      ylab(input$variable_uda) + xlab("Counts") +
      ggtitle(paste("Region histograms", paste(input$variable_uda, input$year_uda, sep = " in the year "), sep = " of the ")) +
      theme(text = element_text(family = 'Gill Sans', color = 'white')
            ,plot.title = element_text(size = 20)
            ,axis.text = element_text(color = 'white')
            ,legend.position= "bottom"
            ,panel.grid = element_blank()
            ,panel.background = element_rect(fill = 'grey30')
            ,plot.background = element_rect(fill = 'grey30')
            ,axis.line = element_line(color = 'white')
            ,legend.background = element_blank()
            ,legend.key = element_blank())
    #Boxplot for region
    p2_reg <- ggplot(data = df_uda, aes(x = region, y = var, fill = region)) +
      geom_boxplot(outlier.colour = 'white', color = 'black' ) +
      ylab(input$variable_uda) +
      ggtitle(paste("Region boxplots", paste(input$variable_uda, input$year_uda, sep = " in the year "), sep = " of the ")) +
      theme(text = element_text(family = 'Gill Sans', color = 'white')
            ,plot.title = element_text(size = 20)
            ,axis.text = element_text(color = 'white')
            ,legend.position= "bottom"
            ,panel.grid = element_blank()
            ,panel.background = element_rect(fill = 'grey30')
            ,plot.background = element_rect(fill = 'grey30')
            ,axis.line = element_line(color = 'white')
            ,legend.background = element_blank()
            ,legend.key = element_blank())
    #Density for region
    p3_reg <- ggplot(data = df_uda, aes(var, fill = region)) +
      geom_density(color = 'black', alpha = 0.3) +
      xlab(input$variable_uda) + ylab("Density") +
      ggtitle(paste("Region densities", paste(input$variable_uda, input$year_uda, sep = " in the year "), sep = " of the ")) +
      theme(text = element_text(family = 'Gill Sans', color = 'white')
            ,plot.title = element_text(size = 20)
            ,axis.text = element_text(color = 'white')
            ,panel.grid = element_blank()
            ,legend.position= "bottom"
            ,panel.background = element_rect(fill = 'grey30')
            ,plot.background = element_rect(fill = 'grey30')
            ,axis.line = element_line(color = 'white')
            ,legend.background = element_blank()
            ,legend.key = element_blank())
    #Conditions to plot each pot
    if(input$group_uda == "Income"){
      if(input$plotype == "Histogram"){
        p1_inc
      }else if(input$plotype == "Boxplot"){
        p2_inc
      }else{
        p3_inc
      }}else if(input$group_uda == "Region"){
        if(input$plotype == "Histogram"){
          p1_reg
        }else if(input$plotype == "Boxplot"){
          p2_reg
        }else{
          p3_reg
        }} else{
          if(input$plotype == "Histogram"){
            p1
          }else if(input$plotype == "Boxplot"){
            p2
          }else{
            p3
          }}
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

# Uploading application
# rsconnect::deployApp()
