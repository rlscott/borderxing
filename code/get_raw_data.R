# US Border Crossing Data

# found on Bureau of Transportation Statistics website, this script written Nov 9, 2023
# https://www.bts.gov/browse-statistical-products-and-data/border-crossing-data/border-crossingentry-data 

# the website you can view the data on
# https://data.bts.gov/Research-and-Statistics/Border-Crossing-Entry-Data/keg4-3bc2/data
border <- readr::read_csv("https://data.bts.gov/api/views/keg4-3bc2/rows.csv?accessType=DOWNLOAD")

# save the csv
readr::write_csv(border, file = "data/raw_border_crossing_entry_data_11_9_23.csv")

# write to a zip file 
zip(zipfile = "data/raw_data_zip", files = "data/raw_border_crossing_entry_data_11_9_23.csv")
