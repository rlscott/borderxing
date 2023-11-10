
# read in the raw data 
border_raw <- readr::read_csv(file = "data/raw_border_crossing_entry_data_11_9_23.csv")

# clean it
# (to do but at least we thought about it!)

library(tidyverse)
border_clean <- border_raw %>%   mutate(
  date_long = lubridate::my(Date),
  Month = month(date_long, label = TRUE),
  Year = year(date_long), 
  Port_name = `Port Name`, 
  Port_code = `Port Code`, 
)


head(border_clean)

# save it (with some selected columns)
border <- border_clean %>% select(Port_name, State, Port_code, Border, Date, 
                                  Month, Year, Measure, Value, Latitude, 
                                  Longitude, Point)

head(border)
save(file = "data/border_clean.Rdata", border)

# load it (for future use)
load("data/border_clean.Rdata")

