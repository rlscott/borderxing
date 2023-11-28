load("data/border_clean.Rdata")
library(tidyverse)

border<-border %>% mutate(obj=ifelse(
  Measure %in% c('Trains','Buses',"Personal Vehicles","Rail Containers Empty",
                 "Truck Containers Loaded","Rail Containers Loaded","Trucks",
                 "Truck Containers Empty"),"Object","People"))

