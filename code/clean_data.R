
# read in the raw data 
border_raw <- readr::read_csv(file = "data/raw_border_crossing_entry_data_11_9_23.csv")

# clean it
# (to do but at least we thought about it!)

border_clean <- border_raw

# save it
border <- border_clean
save(file = "data/border_clean.Rdata", border)

# load it (for future use)
load("data/border_clean.Rdata")

