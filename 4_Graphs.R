##########################################################################################################
#                                         GRAPHS
##########################################################################################################

# Load or install required packages
required_packages <- c("readr", "dplyr", "xts", "zoo")
# Note we need xts --> Since we are dealing with irregular time series!

installed <- required_packages %in% installed.packages()
if (any(!installed)) {
  install.packages(required_packages[!installed])
}
invisible(lapply(required_packages, library, character.only = TRUE))

##########################################################################################################
###                              Graph 1: Coffe prices with futures prices                             ### 
##########################################################################################################


# Read and prepare data
coffee_data <- read_csv("Raw_Data/Coffee_Data_Set.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  arrange(Date)

# Create xts objects indexed by Date
arabica_xts <- xts(coffee_data$Price_Arabica, order.by = coffee_data$Date)
robusta_xts <- xts(coffee_data$Price_Robusta, order.by = coffee_data$Date)
arabica_futures_xts <- xts(coffee_data$Close_USD_60kg, order.by = coffee_data$Date)
usd_real_exchange_xts <- xts(coffee_data$PTAX, order.by = coffee_data$Date)

# Merge all series into one xts object
merged_prices <- merge(arabica_xts, robusta_xts, arabica_futures_xts)
colnames(merged_prices) <- c("Arabica", "Robusta", "Arabica Futures")

# Save the plot as PNG
png("Processed_Data/graph_1_coffee_prices_with_futures.png", width = 1200, height = 600)

# Plot all series on the same graph
plot.zoo(
  merged_prices,
  plot.type = "single",
  col = c("darkgreen", "brown", "blue"),
  lwd = 0.5,
  ylab = "Price (USD/60kg)",
  xlab = "Date",
  main = "Graph 01: Arabica, Robusta, and Arabica Futures Prices"
)

# Add legend
legend("topright", legend = colnames(merged_prices),
       col = c("darkgreen", "brown", "blue"), lty = 1, lwd = 0.5)

# Close the PNG device
dev.off()


##########################################################################################################
##                                         Graph 2: Log Return of Variable of Interest                  ##
##########################################################################################################


# Calculate log returns
arabica_log_returns <- diff(log(arabica_xts))
robusta_log_returns <- diff(log(robusta_xts))
arabica_future_log_returns <- diff(log(arabica_futures_xts))
usd_real_exchange_log_returns <- diff(log(usd_real_exchange_xts))


# Save multiple plots in one PNG file
png("Processed_Data/graph_2_log_coffee_prices_with_futures_exchange_rate.png", width = 1200, height = 800)
par(mfrow = c(2, 2))  # 3 rows, 1 column of plots

# Plot Arabica log returns
plot(na.omit(arabica_log_returns),
     main = "Arabica Coffee Log Returns",
     ylab = "Log Returns",
     col = "darkgreen",
     lwd = 1)

# Plot Robusta log returns
plot(na.omit(robusta_log_returns),
     main = "Robusta Coffee Log Returns",
     ylab = "Log Returns",
     col = "brown",
     lwd = 1)

# Plot Arabica futures log returns
plot(na.omit(arabica_future_log_returns),
     main = "Arabica Futures Log Returns",
     ylab = "Log Returns",
     col = "blue",
     lwd = 1)


# Plot Arabica futures log returns
plot(na.omit(usd_real_exchange_log_returns),
     main = "Exchange Rate Log Returns",
     ylab = "Log Returns",
     col = "purple",
     lwd = 1)

# Close PNG device
dev.off()