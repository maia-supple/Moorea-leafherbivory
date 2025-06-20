# 01_data_cleaning.R
library(tidyverse)

# Load raw data
leafdata_raw <- read_csv("data/raw/DATA COLLECTION_Moorea project - Sheet1.csv")
scoredata_raw <- read_csv("data/raw/DATA COLLECTION_Moorea project - Sheet3.csv")

# Rename for clarity
leafdata <- leafdata_raw
scoredata <- scoredata_raw

# Join datasets by damage_type
leafdata_wscore <- leafdata %>%
  left_join(scoredata %>% select(damage_type, new_score), by = "damage_type")

# Replace NAs with "Undamaged"
leafdata_wscore <- leafdata_wscore %>%
  mutate(
    herbivory_type = ifelse(is.na(Func_feed_group), "Undamaged", Func_feed_group),
    damage_type = ifelse(is.na(damage_type), "Undamaged", damage_type),
    score = ifelse(is.na(new_score), "Undamaged", new_score)
  )

# Save cleaned data for analysis and plotting
saveRDS(leafdata_wscore, "data/processed/leafdata_wscore.rds")
