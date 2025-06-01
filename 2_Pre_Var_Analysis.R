##########################################################################################################
#                                   PRE-VAR ANALYSIS
##########################################################################################################

# Load libraries
library(readr)
library(dplyr)

# Read  CSV file
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
png("Processed_Data/coffee_prices_plot.png", width = 1200, height = 600)

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



#####################################################################################################################

library(xts)
library(dplyr)
library(readr)
library(quantmod)  # for to.weekly()

# Load your data
coffee_data <- read_csv("Raw_Data/combined_coffee_price_index.csv")
coffee_data$Date <- as.Date(coffee_data$Date)

# Filter to dates with both prices > 0 starting 2001-11-08
coffee_filtered <- coffee_data %>%
  filter(Date >= as.Date("2001-11-08")) %>%
  filter(Price_Arabica > 0, Price_Robusta > 0)

# Create xts objects
arabica_xts <- xts(coffee_filtered$Price_Arabica, order.by = coffee_filtered$Date)
robusta_xts <- xts(coffee_filtered$Price_Robusta, order.by = coffee_filtered$Date)

# Calculate daily log returns
arabica_log_returns <- diff(log(arabica_xts))
robusta_log_returns <- diff(log(robusta_xts))

# Aggregate to weekly log returns using period.apply (sum of daily log returns in the week)
# The endpoint function finds the last day of each week
ep <- endpoints(arabica_log_returns, on = "weeks")

arabica_weekly <- period.apply(arabica_log_returns, INDEX = ep, FUN = sum)
robusta_weekly <- period.apply(robusta_log_returns, INDEX = ep, FUN = sum)

# Plot weekly log returns
par(mfrow = c(2,1), mar = c(4,4,3,2))
plot(na.omit(arabica_weekly),
     main = "Arabica Coffee Weekly Log Returns",
     ylab = "Weekly Log Return",
     col = "darkgreen",
     lwd = 2)

plot(na.omit(robusta_weekly),
     main = "Robusta Coffee Weekly Log Returns",
     ylab = "Weekly Log Return",
     col = "brown",
     lwd = 2)


##########################################################################################################




####################################

# Load required packages
library(readr)
library(dplyr)
library(xts)
library(TTR)

# Load the data
coffee_data <- read_csv("Raw_Data/combined_coffee_price_index.csv")

# Convert Date column to Date type
coffee_data$Date <- as.Date(coffee_data$Date)

# Filter valid data starting from 2001-11-08
coffee_filtered <- coffee_data %>%
  filter(Date >= as.Date("2001-11-08")) %>%
  filter(Price_Arabica > 0, Price_Robusta > 0)

# Create xts time series
arabica_xts <- xts(coffee_filtered$Price_Arabica, order.by = coffee_filtered$Date)
robusta_xts <- xts(coffee_filtered$Price_Robusta, order.by = coffee_filtered$Date)

# Weekly aggregation: use closing price of each week
arabica_weekly <- apply.weekly(arabica_xts, last)
robusta_weekly <- apply.weekly(robusta_xts, last)

# Calculate weekly log returns
arabica_weekly_log_returns <- diff(log(arabica_weekly))
robusta_weekly_log_returns <- diff(log(robusta_weekly))

# Plot Arabica weekly log returns for 2014
plot(window(arabica_weekly_log_returns,
            start = as.Date("2014-01-01"),
            end = as.Date("2014-12-31")),
     main = "Arabica Coffee Weekly Log Returns - 2014",
     ylab = "Log Returns",
     xlab = "Week",
     col = "darkgreen",
     lwd = 2)

# Plot Robusta weekly log returns for 2014
plot(window(robusta_weekly_log_returns,
            start = as.Date("2014-01-01"),
            end = as.Date("2014-12-31")),
     main = "Robusta Coffee Weekly Log Returns - 2014",
     ylab = "Log Returns",
     xlab = "Week",
     col = "sienna4",
     lwd = 2)





     plot(window(arabica_xts, 
            start = as.Date("2014-01-01"), 
            end = as.Date("2014-05-01")),
     main = "Arabica Coffee Prices - Jan to May 2014",
     ylab = "Price (USD/lb)",
     xlab = "Date",
     col = "darkgreen",
     lwd = 2)



     plot(window(arabica_weekly_log_returns,
            start = as.Date("2014-01-01"),
            end = as.Date("2014-05-01")),
     main = "Arabica Weekly Log Returns - Jan to May 2014",
     ylab = "Log Returns",
     xlab = "Date",
     col = "blue",
     lwd = 2)
