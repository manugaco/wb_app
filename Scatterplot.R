

#Variables

v1 <- indicators_name[10]
v2 <- indicators_name[1]

var1 <- list[[v1]]
var2 <- list[[v2]]

#Years

years <- seq(1960, 2018, by = 1)
yr <- years[50]

#Merging the dataset in one

var1 <- var1[which(var1$country %nin% setdiff(var1$country, countries)), ]
var1 <- var1 %>% select(country, income, region, which(colnames(var1) == yr))
var2 <- var2[which(var2$country %nin% setdiff(var2$country, countries)), ]
var2 <- var2 %>% select(country, income, region, which(colnames(var2) == yr))

df_vda <- merge(var1, var2, by = "country")
df_vda <- df_vda[, -(5:6)]
colnames(df_vda) <- c("country","income","region", "x", "y")


ggplot(data = df_vda, aes(x=log(x), y=log(y), color = region, size = x)) +
  geom_point() + geom_text(aes(label = country), check_overlap = TRUE, vjust = 1, hjust = 1, color = "white") +
  xlab("") + ylab("") + 
  scale_colour_discrete(name = "region", 
                        breaks = levels(df_vda$region),
                        labels = levels(df_vda$region)) +
  scale_size_continuous(name  = "region", 
                        breaks = levels(df_vda$region),
                        labels = levels(df_vda$region)) +
  theme(text = element_text(family = 'Gill Sans', color = 'white')
        ,plot.title = element_text(size = 20)
        ,axis.text = element_text(color = 'white')
        ,panel.background = element_rect(fill = 'grey30')
        ,plot.background = element_rect(fill = 'grey30')
        ,legend.background = element_blank()
        ,legend.key = element_blank())
  )
