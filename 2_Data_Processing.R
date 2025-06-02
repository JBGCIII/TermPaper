##########################################################################################################
#                                         DATA PROCESSING
##########################################################################################################

library(readr)
library(dplyr)

# Load original data
coffee_data <- read_csv("Raw_Data/Coffee_Data_Set.csv")

# Ensure Date is in Date format
coffee_data$Date <- as.Date(coffee_data$Date)

# Filter date range and create new data frame with logged prices
log_data <- coffee_data %>%
  filter(Date >= as.Date("2001-11-08") & Date <= as.Date("2025-05-27")) %>%
  transmute(
    Date,
    log_PTAX = log(PTAX),
    log_Price_Arabica = log(Price_Arabica),
    log_Price_Robusta = log(Price_Robusta),
    log_Close_USD_60kg = log(Close_USD_60kg)
  )

# Save logged prices (optional)
if (!dir.exists("Processed_Data")) dir.create("Processed_Data")
write_csv(log_data, "Processed_Data/Log_Data.csv")

# Compute log returns by differencing logged prices
log_returns <- log_data %>%
  arrange(Date) %>%
  mutate(
    ret_PTAX = log_PTAX - lag(log_PTAX),
    ret_Arabica = log_Price_Arabica - lag(log_Price_Arabica),
    ret_Robusta = log_Price_Robusta - lag(log_Price_Robusta),
    ret_Close_USD_60kg = log_Close_USD_60kg - lag(log_Close_USD_60kg)
  ) %>%
  filter(!is.na(ret_Arabica))  # Remove first row with NA returns

# Save log returns
write_csv(log_returns, "Processed_Data/Log_and_Log_Returns_Data.csv")