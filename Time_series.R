
#This is the variable selection

vs <- indicators_name[12]
var <- list[[vs]]

#Select the country

count <- seq(1, 217, by = 1)
cnt <- count[122]

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


