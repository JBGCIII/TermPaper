##########################################################################################################
#                                   PRE-VAR ANALYSIS
##########################################################################################################



#########################################GRAPHICAL########################################################

###############################################Plot Arabica and 
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
  filter(Date >= as.Date("2001-11-08")) %>%
  filter(Price_Arabica > 0, Price_Robusta > 0)

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



###
library(readr)

# Load logged data
log_data <- read_csv("Processed_Data/Log_Data.csv")

# Convert Date to Date class if needed
log_data$Date <- as.Date(log_data$Date)

# Define start date for the time series
start_date <- min(log_data$Date)

# Create time series objects for logged variables
log_arabica_ts <- ts(log_data$log_Price_Arabica,
                     start = c(as.numeric(format(start_date, "%Y")),
                               as.numeric(format(start_date, "%j"))),
                     frequency = 365)

log_robusta_ts <- ts(log_data$log_Price_Robusta,
                     start = c(as.numeric(format(start_date, "%Y")),
                               as.numeric(format(start_date, "%j"))),
                     frequency = 365)

log_robusta_futures_ts <- ts(log_data$log_Close_USD_60kg,
                             start = c(as.numeric(format(start_date, "%Y")),
                                       as.numeric(format(start_date, "%j"))),
                             frequency = 365)

log_ptax_ts <- ts(log_data$log_PTAX,
                  start = c(as.numeric(format(start_date, "%Y")),
                            as.numeric(format(start_date, "%j"))),
                  frequency = 365)

# Save plot
png("Processed_Data/log_coffee_prices_and_exchange_rate.png", width = 1200, height = 600)

# Plot logged Arabica price
plot(log_arabica_ts, type = "l", col = "darkgreen", lwd = 1,
     ylab = "Log Price / Log Exchange Rate", xlab = "Time",
     main = "Logged Arabica, Robusta, Robusta Futures Prices and Exchange Rate",
     xaxs = "i",
     ylim = range(c(log_arabica_ts, log_robusta_ts, log_robusta_futures_ts, log_ptax_ts), na.rm = TRUE))

# Add lines for other logged variables
lines(log_robusta_ts, col = "brown", lwd = 1)
lines(log_robusta_futures_ts, col = "blue", lwd = 1)
lines(log_ptax_ts, col = "purple", lwd = 1)

# Add legend
legend("topright",
       legend = c("Log Arabica", "Log Robusta", "Log Robusta Futures", "Log Exchange Rate"),
       col = c("darkgreen", "brown", "blue", "purple"),
       lty = 1, lwd = 1)

# Close PNG device
dev.off()


##############################################################################################################

