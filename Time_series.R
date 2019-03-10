
#This is the variable selection

vs <- indicators_name[12]
var <- list[[vs]]

#Select the country

cnt <- sample(var$country, 1)

#Selecting those countries that are available in the dataset
var = var[which(var$country %nin% setdiff(var$country, countries)), ]

#Deleting countries without position information

no_pos <- c("Channel Islands","Eswatini", "Gibraltar", "Hong Kong SAR, China", "Korea, Dem. People’s Rep.",
            "Macao SAR, China", "Tuvalu", "West Bank and Gaza")

for(i in 1:length(no_pos)){
  var <- var[!var$country == no_pos[i], ]
}

#Selecting the country and formating the input of the plot

rownames(var) <- seq(1, nrow(var), by = 1)
df2 <- var[which(var$country == cnt),]
ts <- t(df2[,-(1:9)])
ts <- cbind(rownames(ts), ts[,1])
colnames(ts) <- c("year", "region")
rownames(ts) <- seq(1, nrow(ts), by = 1)
ts <- data.frame(ts, stringsAsFactors = FALSE)
ts$country <- round(as.numeric(ts[,2]), 3)

#Plotting the time series

plot_ly(ts, x = ~year, y = ~country, type = 'scatter', mode = 'lines', line = list(color = 'blue')) %>%
  layout(yaxis = list(title = vs, color = 'white'),
         xaxis = list(title = 'year', tickangle = 45, color = 'white'),
         plot_bgcolor='rgb(150,150,150)', 
         paper_bgcolor = 'rgb(100,100,100)')
    


