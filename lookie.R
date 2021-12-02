## Snowball sample (Montclair State sampling initiated 11/2/21)

## checking out geographic demography

# demo <- read.csv("lookie.csv")[-c(1:2),]
demo <- read.csv("lookie.12.2.csv")[-c(1:2),]

library(leaflet)

demo$`LocationLongitude` <- round(as.numeric(as.character(demo$`LocationLongitude`)),1)
demo$`LocationLatitude` <- round(as.numeric(as.character(demo$`LocationLatitude`)),1)

## create leaflet map
leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  setView(-98.35, 39.7,
          zoom = 4) %>%
  addTiles() %>%
  addCircles(data=demo, lng = ~`LocationLongitude`, lat = ~`LocationLatitude`, weight = 10)


demo$date <- substr(demo$`RecordedDate`, 1,10)
demo$date <- as.Date(demo$date)

library(ggplot2)

ggplot(demo, aes(x=date)) + geom_histogram(binwidth=1) +
  xlab("") + scale_x_date(breaks = scales::breaks_pretty(10))


########################################################## scales
##########################################################

data <- as.data.frame(lapply(demo, as.numeric))

## UWES (Q14:Q28)

psych::alpha(data[c(30,33,37,41,46)])      ## Vigor


demo$uwes.vigor <- rowMeans(demo[c(30,33,37,41,46)])