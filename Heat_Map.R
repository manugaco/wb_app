
#Libraries

library(shiny)
library(tidyverse)
library(WDI)
library(wbstats)
library(progress)

#This is the variable selection
vs <- indicators_name[10]

#This is the year selected
years <- seq(1960, 2018, by = 1)
yr <- years[55]

var <- list[[vs]]

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

#Removing unnecessary columns

df <- df %>% select(region, which(colnames(df) == yr), group, long, lat)

#Removing extreme values

if(vs == indicators_name[1]){
  df <- df[-(which(df[, (names(df) %in% yr)] > 70000)),]
}

if(vs == indicators_name[4]){
  df <- df[-(which(df[, (names(df) %in% yr)] > 200)),]
}


#Plotting the variablae in the map

fill <- df[, (names(df) %in% yr)]

colors <- c("magma", "plasma", "inferno", "viridis", "cividis")

ggplot() +
  geom_map(data = df, map = df,
           aes(x=long, y=lat, group = group, map_id = region),
           fill="white", colour="black", size=0.5) +
  geom_map(data = df, map = map, aes(fill = fill, map_id = region),
          colour="black", size=0.5) + 
  scale_fill_viridis(option = colors[4], guide = "colorbar",
                     na.value = "grey") +
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(legend.position="bottom", legend.box = "horizontal") + labs(fill=vs) 
