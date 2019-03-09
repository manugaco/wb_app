
#Libraries

library(shiny)
library(tidyverse)
library(WDI)
library(wbstats)
library(progress)

#Loading dataset from World Bank API using the "WDI"

#List of variables of interest (codes and names)

indicators_code <- c("NY.GDP.PCAP.CD", 
                     "NY.GDP.DEFL.KD.ZG", 
                     "BN.GSR.GNFS.CD", 
                     "NE.TRD.GNFS.ZS", 
                     "LP.LPI.INFR.XQ", 
                     "SL.UEM.TOTL.ZS", 
                     "SP.POP.TOTL", 
                     "SL.TLF.TOTL.IN", 
                     "NE.DAB.TOTL.ZS", 
                     "NE.CON.TOTL.ZS",
                     "GC.DOD.TOTL.GD.ZS", 
                     "SP.DYN.LE00.IN", 
                     "SP.DYN.CBRT.IN", 
                     "SP.DYN.CDRT.IN", 
                     "SE.SEC.TCAQ.UP.ZS", 
                     "SE.COM.DURS",
                     "SE.XPD.TOTL.GD.ZS", 
                     "SE.ADT.LITR.ZS", 
                     "SE.PRM.REPT.ZS", 
                     "SE.SEC.PROG.ZS", 
                     "ST.INT.ARVL", 
                     "GC.TAX.IMPT.ZS",
                     "LP.LPI.CUST.XQ", 
                     "TM.VAL.FUEL.ZS.UN", 
                     "TX.VAL.TECH.MF.ZS", 
                     "GB.XPD.RSDV.GD.ZS", 
                     "IP.JRN.ARTC.SC", 
                     "SP.POP.SCIE.RD.P6", 
                     "IP.PAT.RESD", 
                     "IP.PAT.NRES")

indicators_name <- c("GDP per capita (current US$)",
                     "Inflation, GDP deflator (annual %)",
                     "Net trade in goods and services (BoP, current US$)",
                     "Trade (% of GDP)",
                     "Logistics performance index: Quality of trade and transport-related infrastructure (1=low to 5=high)",
                     "Unemployment, total (% of total labor force) (modeled ILO estimate)",
                     "Population, total",
                     "Labor force, total",
                     "Gross national expenditure (% of GDP)",
                     "Final consumption expenditure (% of GDP)",
                     "Central government debt, total (% of GDP)",
                     "Life expectancy at birth, total (years)",
                     "Birth rate, crude (per 1,000 people)",
                     "Death rate, crude (per 1,000 people)",
                     "Trained teachers in upper secondary education (% of total teachers)",
                     "Compulsory education, duration (years)",
                     "Government expenditure on education, total (% of GDP)",
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

#Creating the list with the variables of the dataset.

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

