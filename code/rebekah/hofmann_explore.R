load("data/border_clean.Rdata")

library(tidyverse)

names(border)

border %>%
  group_by(Port_name, Month, Year) %>% 
  summarize(
    Value = sum(Value)
  ) %>%
  ggplot(aes(x = Year + (as.numeric(Month)-1)/12, y = Value, label=Port_name)) +
  geom_point(aes(colour=factor(Month)))

plotly::ggplotly()  


border %>%
  group_by(Port_name, Month, Year ) %>% 
  summarize(
    Value = sum(Value)
  ) %>% filter(Port_name %in% c("San Ysidro", "El Paso")) %>%
  ggplot(aes(x = Year + (as.numeric(Month)-1)/12, y = Value, label=Port_name)) +
  geom_point(aes(colour=factor(Month))) +
  facet_wrap(~Port_name)


stations <- unique(border %>% select(Port_code, Port_name, Longitude, Latitude))  



library(leaflet)

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
m    

k <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=-93.6506941, lat=42.0290807, popup="The birthplace of knitr")

k

leaflet(stations) %>%
  addTiles() %>%
  addCircleMarkers(~Longitude, ~Latitude, popup=~Port_name, radius=3)

border %>%
  ggplot(aes(x = Port_name, y = Value, fill = Measure)) + 
  geom_bar(stat = "identity") + coord_flip()


border %>% filter(Border == "US-Mexico Border") %>% 
  ggplot(aes(x = reorder(Port_name, as.numeric(Measure), na.rm = TRUE), fill = Measure)) + 
  geom_bar(position = "fill") + coord_flip()


border %>% filter(Port_name == "Cross Border Xpress")
# reorder(state_abbr, rate, 
#         FUN = median, na.rm = TRUE)
