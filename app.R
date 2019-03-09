#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(WDI)
library(wbstats)
library(progress)

#Loading dataset from World Bank API using the "WDI"

#Vqriables of interest (codes and names)

indicators_code <- c("NY.GDP.PCAP.CD", "NY.GDP.DEFL.KD.ZG", "BN.GSR.GNFS.CD", "NE.TRD.GNFS.ZS", 
                     "LP.LPI.INFR.XQ", "SL.UEM.TOTL.ZS", "SP.POP.TOTL", "SL.TLF.TOTL.IN", "NE.DAB.TOTL.ZS", "NE.CON.TOTL.ZS",
                     "GC.DOD.TOTL.GD.ZS", "SP.DYN.LE00.IN", "SP.DYN.CBRT.IN", "SP.DYN.CDRT.IN", "SE.SEC.TCAQ.UP.ZS", "SE.COM.DURS",
                     "SE.XPD.TOTL.GD.ZS", "SE.ADT.LITR.ZS", "SE.PRM.REPT.ZS", "SE.SEC.PROG.ZS", "ST.INT.ARVL", "GC.TAX.IMPT.ZS",
                     "LP.LPI.CUST.XQ", "TM.VAL.FUEL.ZS.UN", "TX.VAL.TECH.MF.ZS", "GB.XPD.RSDV.GD.ZS", "IP.JRN.ARTC.SC", 
                     "SP.POP.SCIE.RD.P6", "IP.PAT.RESD", "IP.PAT.NRES")

indicators_name <- c("GDP per capita (current US$)","Gross national expenditure (% of GDP)", "Inflation, GDP deflator (annual %)", 
                     "Final consumption expenditure (% of GDP)", "Central government debt, total (% of GDP)",
                     "Unemployment, total (% of total labor force) (modeled ILO estimate)",
                     "Population, total", "Labor force, total", "Life expectancy at birth, total (years)", "Birth rate, crude (per 1,000 people)",
                     "Death rate, crude (per 1,000 people)",
                     "Net trade in goods and services (BoP, current US$)", "Trade (% of GDP)","Logistics performance index: Quality of trade and transport-related infrastructure (1=low to 5=high)",
                     "Government expenditure on education, total (% of GDP)",
                     "Trained teachers in upper secondary education (% of total teachers)",
                     "Compulsory education, duration (years)",
                     "Literacy rate, adult total (% of people ages 15 and above)",
                     "Repeaters, primary, total (% of total enrollment)",
                     "Progression to secondary school (%)",
                     "International tourism, number of arrivals",
                     "Customs and other import duties (% of tax revenue)",
                     "Logistics performance index: Efficiency of customs clearance process (1=low to 5=high)",
                     "Fuel imports (% of merchandise imports)",
                     "High-technology exports (% of manufactured exports)",
                     "Research and development expenditure (% of GDP)",
                     "Scientific and technical journal articles",
                     "Researchers in R&D (per million people)",
                     "Patent applications, residents",
                     "Patent applications, nonresidents")

#Creating the list with the variables

list <- list()
pb <- progress_bar$new(total = 30)

for(i in 1:length(indicators_code)){
  pb$tick()
  indicator <- indicators_name[i]
  var <- WDI(country = "all", indicator = c(indicator =  indicators_code[i]), start = 1960,
             end = 2018, extra = TRUE)
  list[[i]] <- var %>% spread(year, indicator)
}

#Naming the list and selecting country names only

names(list) <- indicators_name

countries_ls <- wbcountries(lang = 'en')
countries <- countries_ls$country[!is.na(countries_ls$regionID)]

#Setting up the dataset (GDPpc for instance)

var <- list[[1]]
var <- var[var$country %in% countries,]


df <- merge(var, map, sort = FALSE, by = "country")
df <- df[order(df$order), ]

nrow(unique(df))

#Plotting the 

ggplot(df, aes(longitude, latitude)) +
  geom_polygon(aes(group = group, fill = assault))


USArrests

states <- map_data("state")
arrests <- USArrests
names(arrests) <- tolower(names(arrests))
arrests$region <- tolower(rownames(USArrests))

choro <- merge(states, arrests, sort = FALSE, by = "country")
choro <- choro[order(choro$order), ]


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

