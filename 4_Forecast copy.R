##########################################################################################################
#                                   VAR & VECM MODEL
##########################################################################################################


# Package installation
# Load or install required packages
required_packages <- c("readr", "dplyr", "xts", "urca", "dynlm", "vars", "forecast")

installed <- required_packages %in% installed.packages()
if (any(!installed)) {
  install.packages(required_packages[!installed])
}
invisible(lapply(required_packages, library, character.only = TRUE))

# Data preparation #
# 1. Read CSV and preprocess
coffee_data <- read_csv("Raw_Data/Coffee_Data_Set.csv")

# 2. Create xts objects
arabica_spot_xts <- xts(coffee_data$Price_Arabica, order.by = coffee_data$Date)
robusta_spot_xts <- xts(coffee_data$Price_Robusta, order.by = coffee_data$Date)
arabica_futures_xts <- xts(coffee_data$Close_USD_60kg, order.by = coffee_data$Date)
usd_real_exchange_xts <- xts(coffee_data$PTAX, order.by = coffee_data$Date)

# 3. Merge series on common dates
# Merge the four xts objects with inner join on common dates
data_xts <- merge(arabica_spot_xts, robusta_spot_xts, arabica_futures_xts, usd_real_exchange_xts, join = "inner")

# Rename columns
colnames(data_xts) <- c("arabica_spot_price", "robusta_spot_price", "arabica_futures_price", "usd_real_exchange")


diff_log_spot_price_arabica <- na.omit(diff(log(arabica_spot_xts)))
diff_log_spot_price_robusta <- na.omit(diff(log(robusta_spot_xts)))
diff_log_futures_price_arabica <- na.omit(diff(log(arabica_futures_xts)))
diff_log_usd_real_exchange <- na.omit(diff(log(usd_real_exchange_xts)))



# Save PNG
png("Processed_Data/graph_1_coffee_prices_with_futures.png", width = 1200, height = 600)

# Plot
plot.zoo(
  merged_prices,
  plot.type = "single",
  col = c("darkgreen", "brown", "blue"),
  lwd = 0.5,
  ylab = "Price (USD/60kg)",
  xlab = "Date",
  main = "Graph 01: Arabica, Robusta, and Arabica Futures Prices"
)

legend("topright", legend = colnames(merged_prices),
       col = c("darkgreen", "brown", "blue"), lty = 1, lwd = 0.5)

dev.off()

##########################################################################################################
###                               2. ARIMA                                                             ### 


# 1. ARIMA forecast (white noise model)
log_futures <- log(xts(data$Close_USD_60kg, order.by = data$Date))
train_end <- as.Date("2014-12-31")
test_end <- as.Date("2015-01-10")

train <- window(log_futures, end = train_end)
test  <- window(log_futures, start = train_end + 1, end = test_end)

arima_model <- Arima(train, order = c(0, 1, 0))
arima_fc <- forecast(arima_model, h = length(test))
fc_prices <- exp(arima_fc$mean)
fc_xts <- xts(fc_prices, order.by = index(test))

# 2 Plot ARIMA forecast
png("Processed_Data/graph_8_Forecast_ARIMA.png", width = 1200, height = 800)
plot(exp(test), main = "Futures Price: Actual vs Forecast (Drift)",
     col = "blue", lwd = 2, ylab = "Price", xlab = "Date")
lines(fc_xts, col = "red", lwd = 2, lty = 2)
legend("topleft", legend = c("Actual", "Forecast with Drift"),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)
# Close PNG device
dev.off()