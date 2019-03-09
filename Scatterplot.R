
#Libraries

library(shiny)
library(tidyverse)
library(WDI)
library(wbstats)
library(progress)

#Variables

v1 <- indicators_name[12]
v2 <- indicators_name[3]

var1 <- list[[v1]]
var2 <- list[[v2]]

#Years

years <- seq(1960, 2018, by = 1)
yr <- years[55]

#Merging the dataset in one

countries_ls <- wbcountries(lang = 'en')
countries <- countries_ls$country[!is.na(countries_ls$regionID)]
var1 <- var1[which(var1$country %nin% setdiff(var1$country, countries)), ]
var1 <- var1 %>% select(country, income, region, which(colnames(var1) == yr))

countries_ls <- wbcountries(lang = 'en')
countries <- countries_ls$country[!is.na(countries_ls$regionID)]
var2 <- var2[which(var2$country %nin% setdiff(var2$country, countries)), ]
var2 <- var2 %>% select(country, income, region, which(colnames(var2) == yr))

df <- merge(var1, var2, by = "country")
df <- df[, -(5:6)]
colnames(df) <- c("country","income","region", "x", "y")

paste(v1, v2, sep = " vs ")

ggplot(data = df, aes(x=x, y=y, color=region, size= x)) +
  geom_point() +
  ggtitle(paste(v2, v1, sep = " vs "))

