## Shiny App

_Antonio Polo de Alvarado - Manuel García Corbí_

_Master in Statistics for Data Science_
_2018-2019_

The purpose of this project is to create a Shiny app using data from the Word Bank website, in order to create visualizations about economic and demographic data.

The data that we are going to use is made up with information about topics such as Ecomomic growth, Demographics and education, in particular:

 - Economy and Growth: GDPpc, Inflation, Trade Balance, Goverment debt, consumption and expenses.
 - Demography: Total population, active population, unemployment rate, life expectancy, birth rate and death rate.
 - Education: Education expenses, compulsory education, literacy rate, secondary transitions, trained teachers and repeaters.
 - Trade: Goods trade, tools duties, import terms, international tourism, logistic performance and taxes.
 - Science and tech: High-tech exports, R&D expenses, papers published, Researchers, patents request and intellectual property costs.

The app contains the following information:

 - First section: Brief description about the app and source of information
 - Second section: Time Series chart, including up to three different countries, one variable
 - Third section: Heat Map, representing one variable with the possibility to choose different years
 - Fourth section: Animated chart, useful to compare two variables aling time
 - Fifth section: Exploratory Analysis, includes different non-parametric visualizations.
 - Sixth section: Info, link to this repository and information about the authors
 - Seventh section: Settings, change the app style.

The app can be visualize using this link:

https://manugaco.shinyapps.io/project/


**_Information about the World Bank API:_**
_The script "Data.R" downloads the world bank dataset by means of the world bank API. However it takes too much time to download the dataset, this is  because of the data structure. So the "env.Rdata" file is prepared to set the dataset into the environment instantly._
