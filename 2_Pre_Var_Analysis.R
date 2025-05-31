##########################################################################################################
#                                   PRE-VAR ANALYSIS
##########################################################################################################

# Load libraries
library(readr)
library(dplyr)

# Read your CSV file (replace with your actual file path)
coffee_data <- read_csv("Raw_Data/combined_coffee_price_index.csv")

# Convert Date to Date class
coffee_data <- coffee_data %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

# Filter out rows where either price is NA (align start date)
coffee_data_aligned <- coffee_data %>%
  filter(!is.na(Price_Arabica) & !is.na(Price_Robusta))

# Check start and end date
start_date <- min(coffee_data_aligned$Date)
end_date <- max(coffee_data_aligned$Date)
cat("Data starts on:", start_date, "\nData ends on:", end_date, "\n")

# Create numeric time index from start date (for daily data)
coffee_data_aligned <- coffee_data_aligned %>%
  arrange(Date) %>%
  mutate(TimeIndex = as.numeric(Date - start_date) + 1)

# Convert prices to time series objects
arabica_ts <- ts(coffee_data_aligned$Price_Arabica,
                 start = c(as.numeric(format(start_date, "%Y")),
                           as.numeric(format(start_date, "%j"))),  # day of year
                 frequency = 365)

robusta_ts <- ts(coffee_data_aligned$Price_Robusta,
                 start = c(as.numeric(format(start_date, "%Y")),
                           as.numeric(format(start_date, "%j"))),
                 frequency = 365)

# Save a wider plot as PNG
png("Processed_Data/coffee_prices_wide_plot.png", width = 1200, height = 600)

# Plot Arabica prices
plot(arabica_ts, type = "l", col = "darkgreen", lwd = 2,
     ylab = "Price (USD/lb)", xlab = "Time",
     main = "Arabica vs Robusta Coffee Prices",
     xaxs = "i")

# Add Robusta prices line
lines(robusta_ts, col = "brown", lwd = 2)

# Add legend
legend("topright", legend = c("Arabica", "Robusta"),
       col = c("darkgreen", "brown"), lty = 1, lwd = 2)

# Close the PNG device to save the file
dev.off()