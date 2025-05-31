##########################################################################################################
#                           DATA PROCESSING
##########################################################################################################
# In this code I make data ready for analysis.


# Load required libraries
library(readxl)
library(dplyr)
library(lubridate)

# --- Load raw Excel files ---
# The skip = 3 is neccesary as the value start from the 3rd row
arabica_raw <- read_excel("Raw_Data/Arabica Coffe Price Index.xlsx", skip = 3)
robusta_raw <- read_excel("Raw_Data/Robusta Coffe Price Index.xlsx", skip = 3)

# --- Clean Arabica ---
arabica <- arabica_raw %>%
  filter(!is.na(Date), !is.na(`Price US$`)) %>%
  mutate(Date = mdy(Date)) %>%  # Use dmy() if date format is dd/mm/yyyy
  distinct(Date, .keep_all = TRUE) %>%
  rename(Price_Arabica = `Price US$`)

# --- Clean Robusta ---
robusta <- robusta_raw %>%
  filter(!is.na(Date), !is.na(`Price US$`)) %>%
  mutate(Date = mdy(Date)) %>%
  distinct(Date, .keep_all = TRUE) %>%
  rename(Price_Robusta = `Price US$`)

# --- Join on matching dates only ---
combined_df <- inner_join(arabica, robusta, by = "Date")

# --- Save the final merged dataset to CSV ---
write.csv(combined_df, "Raw_Data/combined_coffee_price_index.csv", row.names = FALSE)

# --- Print result summary ---
cat("✅ Success! Merged file with", nrow(combined_df), "matched dates saved as 'combined_coffee_price_index.csv'\n")


# Read combined CSV
combined_df <- read.csv("Raw_Data/combined_coffee_price_index.csv", stringsAsFactors = FALSE)
