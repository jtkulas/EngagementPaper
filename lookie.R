## checking out geographic demography

demo <- read.csv("lookie.csv")[-c(1:2),]

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
