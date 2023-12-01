load("data/border_clean.Rdata")

border$Date
min(border$Date)

levels(border$Date)

border %>% ggplot(aes(x = Date, y = Value, fill = Measure)) + geom_point()


# leaflet stuffs

stations <- unique(border %>% select(Port_code, Port_name, Longitude, Latitude) %>% filter(Port_name != "Anchorage"))  

library(leaflet)

png("code/rebekah/help.png")
leaflet(stations) %>%
  addTiles() %>%
  addCircleMarkers(~Longitude, ~Latitude, popup=~Port_name, radius=3)
dev.off()
