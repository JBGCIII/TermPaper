#############################################################################
#                                VAR & ARIMA MODEL
#############################################################################

# 1. Load required packages
packages <- c("readr", "dplyr", "xts", "vars", "urca", "forecast")
lapply(packages[!packages %in% installed.packages()], install.packages)
invisible(lapply(packages, library, character.only = TRUE))

# 2. Load and preprocess data
data <- read_csv("Raw_Data/Coffee_Data_Set.csv") %>%
  mutate(Date = as.Date(Date))

# 3. Function to create log-differenced xts series
log_diff_xts <- function(column) {
  xts_data <- xts(data[[column]], order.by = data$Date)
  na.omit(diff(log(xts_data)))
}

# 4. Create log-differenced series for VAR model
diff_series <- list(
  arabica = log_diff_xts("Price_Arabica"),
  robusta = log_diff_xts("Price_Robusta"),
  futures = log_diff_xts("Close_USD_60kg"),
  usd_brl = log_diff_xts("PTAX")
)

# 5. Combine all into one time series object
data_ts <- na.omit(do.call(merge, diff_series))
colnames(data_ts) <- c("arabica", "robusta", "futures", "usd_brl")

#############################################################################
###                               1. VAR
#############################################################################

# 1. Select optimal lag using AIC
lag_opt <- VARselect(data_ts, lag.max = 10, type = "const")$selection["AIC(n)"]
var_model <- VAR(data_ts, p = lag_opt, type = "const")

# 2. Forecast with VAR
h_forecast <- 14
var_fc <- predict(var_model, n.ahead = h_forecast)

# 3. Recover arabica price forecast (log return to price)
last_log_price <- log(tail(data$Price_Arabica, 1))
cum_log_fc <- cumsum(var_fc$fcst$arabica[,1])
forecast_log <- last_log_price + cum_log_fc
forecast_price <- exp(forecast_log)

# 4. Plot forecast
forecast_dates <- seq(max(data$Date) + 1, by = "day", length.out = h_forecast)
forecast_xts <- xts(forecast_price, order.by = forecast_dates)

# Save VAR plots to PNG
png("Processed_Data/graph_7_Forecast_VAR.png", width = 1200, height = 800)
par(mfrow = c(3, 1))
plot(forecast_dates, var_fc$fcst$arabica[,1], type = "l", col = "blue", lwd = 2,
     main = "Forecast: Arabica", ylab = "Log Diff", xlab = "Date"); grid()
plot(forecast_dates, var_fc$fcst$robusta[,1], type = "l", col = "red", lwd = 2,
     main = "Forecast: Robusta", ylab = "Log Diff", xlab = "Date"); grid()
plot(forecast_dates, var_fc$fcst$usd_brl[,1], type = "l", col = "darkgreen", lwd = 2,
     main = "Forecast: USD/BRL", ylab = "Log Diff", xlab = "Date"); grid()
par(mfrow = c(1, 1))
dev.off()

#############################################################################
###                               2. ARIMA
#############################################################################

#############################################################################
###                               2. ARIMA
#############################################################################
library(xts)
library(forecast)

# 1. Create log-level futures price series
log_futures <- xts(log(data$Close_USD_60kg), order.by = data$Date)

# 2. Split into train and test sets
train_end <- as.Date("2014-12-31")
test_end  <- as.Date("2015-01-10")

train <- log_futures[index(log_futures) <= train_end]
test  <- log_futures[index(log_futures) > train_end & index(log_futures) <= test_end]

# 3. Fit ARIMA(0,1,0) model (random walk without drift)
arima_model <- Arima(train, order = c(0, 1, 0), include.drift = FALSE)

# 4. Forecast for the length of the test set
arima_fc <- forecast(arima_model, h = length(test))

# 5. Convert forecast back to price level
fc_prices <- exp(arima_fc$mean)
fc_xts <- xts(fc_prices, order.by = index(test))

# 6. Align dates - intersection only
common_dates <- intersect(index(test), index(fc_xts))

# Extract data to plot
actual_prices <- as.numeric(exp(test[common_dates]))
forecast_prices <- as.numeric(fc_xts[common_dates])
plot_dates <- common_dates

# Debug prints
print(paste("Actual prices:", paste(round(actual_prices, 2), collapse = ", ")))
print(paste("Forecast prices:", paste(round(forecast_prices, 2), collapse = ", ")))
print(paste("Common dates:", paste(as.character(plot_dates), collapse = ", ")))

# 7. Plot and save as PDF (you can change to PNG if you want)
pdf("Processed_Data/graph_8_Forecast_ARIMA.pdf", width = 12, height = 8)

tryCatch({
  plot(plot_dates, actual_prices, type = "l", col = "blue", lwd = 2,
       main = "Futures Price: Actual vs Forecast (Random Walk no Drift)",
       ylab = "Price", xlab = "Date")
  lines(plot_dates, forecast_prices, col = "red", lwd = 2, lty = 2)
  legend("topleft", legend = c("Actual", "Forecast"),
         col = c("blue", "red"), lty = c(1, 2), lwd = 2)
}, error = function(e) {
  message("Plot error: ", e$message)
})

dev.off()