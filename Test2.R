
library(quantmod)
library(dplyr)
library(lubridate)

# Define start and end dates
start_date <- as.Date("2001-01-01")
end_date <- Sys.Date()

# Download Arabica Coffee Futures daily data from Yahoo Finance
arabica_futures <- getSymbols("KC=F", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)

# Convert to data.frame and clean up
arabica_df <- data.frame(
  Date = index(arabica_futures),
  coredata(arabica_futures)
) %>%
  rename(
    Open = KC.F.Open,
    High = KC.F.High,
    Low = KC.F.Low,
    Close = KC.F.Close,
    Volume = KC.F.Volume,
    Adjusted = KC.F.Adjusted
  )

# Check first rows
head(arabica_df)

# Save to CSV
write.csv(arabica_df, "Raw_Data/arabica_coffee_futures.csv", row.names = FALSE)

cat("✅ Arabica coffee futures daily prices saved to 'Raw_Data/arabica_coffee_futures.csv'\n")











# If you load the CQI dataset:
library(tidyverse)

# Summary of coffee data
summary(coffee_data)

# Plot by country
coffee_data %>%
  ggplot(aes(x = Country.of.Origin, y = Total.Cup.Points)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
