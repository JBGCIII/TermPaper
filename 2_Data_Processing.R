##########################################################################################################
#                                         DATA PROCESSING
##########################################################################################################

library(readr)
library(dplyr)

# Load original data
coffee_data <- read_csv("Raw_Data/Coffee_Data_Set.csv")

# Ensure Date is in Date format
coffee_data$Date <- as.Date(coffee_data$Date)

# Filter date range and create new data frame with only Date and logged variables
log_data <- coffee_data %>%
  filter(Date >= as.Date("2001-11-08") & Date <= as.Date("2025-05-27")) %>%
  transmute(
    Date,
    log_PTAX = log(PTAX),
    log_Price_Arabica = log(Price_Arabica),
    log_Price_Robusta = log(Price_Robusta),
    log_Close_USD_60kg = log(Close_USD_60kg)
  )

# Make sure the output directory exists
if (!dir.exists("Processed_Data")) dir.create("Processed_Data")

# Save to CSV
write_csv(log_data, "Processed_Data/Log_Data.csv")

######################################################################################################3

