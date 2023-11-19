
# read in the raw data 
border_raw <- readr::read_csv(file = "data/raw_border_crossing_entry_data_11_9_23.csv")

# clean it (notice that there are two Eastports in different states)

library(tidyverse)
border_clean <- border_raw %>%   mutate(
  date_long = lubridate::my(Date),
  Month = month(date_long, label = TRUE),
  Year = year(date_long), 
  # Port_name = as.factor(`Port Name`), 
  Port_name = as.factor(case_when(
    `Port Name` == "Eastport" & `Port Code` == "3302" ~ "Eastport ID", 
    `Port Name` == "Eastport" & `Port Code` == "0103" ~ "Eastport ME", 
    .default = `Port Name`)),
  Port_code = as.factor(`Port Code`), 
  State = as.factor(State), 
  Border = as.factor(Border), 
  Measure = as.factor(Measure), 
  Type = as.factor(ifelse(
    Measure %in% c('Trains','Buses',"Personal Vehicles","Rail Containers Empty",
                   "Truck Containers Loaded","Rail Containers Loaded","Trucks",
                   "Truck Containers Empty"),"Object","People"))
  ) 

# checks 
head(border_clean)
border_clean %>% select(Port_name, Port_code, State) %>% 
  unique() # %>% filter(Port_code %in% c("3302","0103"))

# save it (with some selected columns)
border <- border_clean %>% select(Port_name, State, Port_code, Border, Date, 
                                  Month, Year, Measure, Value, Type, Latitude, 
                                  Longitude, Point)
head(border)

# head(border)
save(file = "data/border_clean.Rdata", border)

# load it (for future use)
# load("data/border_clean.Rdata")
# head(border)
