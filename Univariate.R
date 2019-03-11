
var_uda <- list[[indicators_name[4]]]

#Years

years <- seq(1960, 2018, by = 1)
yr_uda <- years[45]

#Merging the data in one dataset

var_uda <- var_uda[which(var_uda$country %nin% setdiff(var_uda$country, countries)), ]
df_uda <- var_uda %>% select(country, income, region, which(colnames(var_uda) == yr_uda))

df_uda <- df_uda[, -(5:6)]
colnames(df_uda) <- c("country","income","region", "var")

p1 <- ggplot(data = df_uda, aes(log(var), fill = income)) +
  geom_histogram(color = 'black') +
  theme(text = element_text(family = 'Gill Sans', color = 'white')
        ,plot.title = element_text(size = 20)
        ,axis.text = element_text(color = 'white')
        ,panel.grid = element_blank()
        ,panel.background = element_rect(fill = 'grey30')
        ,plot.background = element_rect(fill = 'grey30')
        ,axis.line = element_line(color = 'white')
        ,legend.background = element_blank()
        ,legend.key = element_blank())

p2 <- ggplot(data = df_uda, aes(x = income, y = log(var), fill = income)) +
  geom_boxplot(outlier.colour = 'white', color = 'black' ) +
  coord_flip() + ylab("") +
  theme(text = element_text(family = 'Gill Sans', color = 'white')
        ,plot.title = element_text(size = 20)
        ,axis.text = element_text(color = 'white')
        ,axis.text.y = element_blank()
        ,panel.grid = element_blank()
        ,panel.background = element_rect(fill = 'grey30')
        ,plot.background = element_rect(fill = 'grey30')
        ,axis.line = element_line(color = 'white')
        ,legend.background = element_blank()
        ,legend.key = element_blank())

p3 <- ggplot(data = df_uda, aes(log(var), fill = income)) +
  geom_density(color = 'black', alpha = 0.3) +
  theme(text = element_text(family = 'Gill Sans', color = 'white')
        ,plot.title = element_text(size = 20)
        ,axis.text = element_text(color = 'white')
        ,panel.grid = element_blank()
        ,panel.background = element_rect(fill = 'grey30')
        ,plot.background = element_rect(fill = 'grey30')
        ,axis.line = element_line(color = 'white')
        ,legend.background = element_blank()
        ,legend.key = element_blank())

gridExtra::grid.arrange(p1, p2, p3)




