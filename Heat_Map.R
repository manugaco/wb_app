
#Libraries

library(shiny)
library(tidyverse)
library(WDI)
library(wbstats)
library(progress)
library(Hmisc)
library(maps)
library(viridis)
library(mapproj)

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
