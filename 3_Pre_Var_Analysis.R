##########################################################################################################
#                                   PRE-VAR ANALYSIS
##########################################################################################################

# Load libraries
library(readr)
library(dplyr)

# Read CSV file (correct filename)
coffee_data <- read_csv("Raw_Data/Coffee_Data_Set.csv")

# Convert Date to Date class
coffee_data <- coffee_data %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

# Filter out rows with any NA in key price columns
coffee_data_aligned <- coffee_data %>%
  filter(!is.na(Price_Arabica) & !is.na(Price_Robusta) & !is.na(Close_USD_60kg))

# Check start and end dates
start_date <- min(coffee_data_aligned$Date)
end_date <- max(coffee_data_aligned$Date)
cat("Data starts on:", start_date, "\nData ends on:", end_date, "\n")

# Time index
coffee_data_aligned <- coffee_data_aligned %>%
  arrange(Date) %>%
  mutate(TimeIndex = as.numeric(Date - start_date) + 1)

# Create time series objects
arabica_ts <- ts(coffee_data_aligned$Price_Arabica,
                 start = c(as.numeric(format(start_date, "%Y")),
                           as.numeric(format(start_date, "%j"))),
                 frequency = 365)

robusta_ts <- ts(coffee_data_aligned$Price_Robusta,
                 start = c(as.numeric(format(start_date, "%Y")),
                           as.numeric(format(start_date, "%j"))),
                 frequency = 365)

robusta_futures_ts <- ts(coffee_data_aligned$Close_USD_60kg,
                         start = c(as.numeric(format(start_date, "%Y")),
                                   as.numeric(format(start_date, "%j"))),
                         frequency = 365)

# Save plot
png("Processed_Data/coffee_prices_with_futures.png", width = 1200, height = 600)

# Plot Arabica
plot(arabica_ts, type = "l", col = "darkgreen", lwd = 1,
     ylab = "Price (USD/60kg)", xlab = "Time",
     main = "Arabica, Robusta, and Robusta Futures Prices",
     xaxs = "i", ylim = range(c(arabica_ts, robusta_ts, robusta_futures_ts), na.rm = TRUE))

# Add Robusta and Futures
lines(robusta_ts, col = "brown", lwd = 1)
lines(robusta_futures_ts, col = "blue", lwd = 1)

# Legend
legend("topright", legend = c("Arabica", "Robusta", "Robusta Futures"),
       col = c("darkgreen", "brown", "blue"), lty = c(1, 1, 1), lwd = 1)

# Close PNG device
dev.off()





##########################################################################################################


# Load libraries
library(readr)
library(dplyr)
library(xts)

# Read data
coffee_data <- read_csv("Raw_Data/combined_coffee_price_index.csv")

# Convert Date column to Date class
coffee_data$Date <- as.Date(coffee_data$Date)

# Filter for dates with both prices available and positive, starting from 2001-11-08
coffee_filtered <- coffee_data %>%
  filter(Date >= as.Date("2001-11-08")) %>%
  filter(Price_Arabica > 0, Price_Robusta > 0)

# Create xts time series objects for prices
arabica_xts <- xts(coffee_filtered$Price_Arabica, order.by = coffee_filtered$Date)
robusta_xts <- xts(coffee_filtered$Price_Robusta, order.by = coffee_filtered$Date)

# Calculate log returns (daily)
arabica_log_returns <- diff(log(arabica_xts))
robusta_log_returns <- diff(log(robusta_xts))

# Plot Arabica log returns (remove NAs)
plot(na.omit(arabica_log_returns),
     main = "Arabica Coffee Log Returns",
     ylab = "Log Returns",
     col = "darkgreen",
     lwd = 1)

# Plot Robusta log returns (remove NAs)
plot(na.omit(robusta_log_returns),
     main = "Robusta Coffee Log Returns",
     ylab = "Log Returns",
     col = "brown",
     lwd = 1)


