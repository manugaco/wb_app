


#This is the variable selection
vs <- indicators_name[1]

#This is the year selected
year <- seq(1960, 2018, by = 1)
yr <- year[55]

var <- list[[vs]]

countries_ls <- wbcountries(lang = 'en')
#Removing NAs
countries <- countries_ls$country[!is.na(countries_ls$regionID)]

#Removing aggregates
var = var[which(var$country %nin% setdiff(var$country, countries)), ]

#Deleting countries without position information

no_pos <- c("Channel Islands","Eswatini", "Gibraltar", "Hong Kong SAR, China", "Korea, Dem. People’s Rep.",
            "Macao SAR, China", "Tuvalu", "West Bank and Gaza")

for(i in 1:length(no_pos)){
  var <- var[!var$country == no_pos[i], ]
}

#Extracting the countries list from world map package
map <- map_data('world')
map <- fortify(map)

#There are some country names that does not match, some tidy is necessary

bad <- c("Antigua", "Bahamas", "Virgin Islands", "Brunei", "Cape Verde", 
         "Democratic Republic of the Congo", "Republic of Congo", "Ivory Coast", "Egypt", "Gambia",
         "Iran", "North Korea", "Kyrgyzstan", "Laos", "Macedonia", "Micronesia", "Russia", "Sint Marteen",
         "Slovakia", "Saint Kitts", "Saint Lucia", "Saint Martin", "Saint Vincent", "Syria", "Trinidad",
         "UK", "USA", "Venezuela", "Virgin Islands", "Yemen")

good <- sort(setdiff(var$country, map$region))

for(i in 1:length(bad)){
  map <- map %>% mutate(region = if_else(region == bad[i], good[i], region))
}

df <- left_join(map, var, by = c('region' = 'country'))
df <- df[!duplicated(df$region), ]

#Removing extreme values

if(vs == indicators_name[1]){
  df <- df[-(which(df[,yr] > 70000)),]
}

if(vs == indicators_name[4]){
  df <- df[-(which(df[, (names(df) %in% yr)] > 200)),]
}

#Plotting the variablae in the map

fill <- df[, (names(df) %in% yr)]
mid = mean(na.omit(fill))
colors <- c("blue", "red", "orange", "green")

ggplot() +
  geom_map(data = map, map = map,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "white", colour = "black", size=0.5) +
  geom_map(data = df, map = map, aes(fill = fill, map_id = region),
           colour="black", size = 0.5) +
  scale_color_gradientn(colours = rainbow(5)) +
  theme_map() +
  ggtitle(vs) + labs(fill=vs)
