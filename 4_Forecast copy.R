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

# 1. Create log-level futures price series
log_futures <- xts(log(data$Close_USD_60kg), order.by = data$Date)

# 2. Split into train and test sets
train_end <- as.Date("2014-12-31")
test_end  <- as.Date("2015-01-10")

train <- window(log_futures, end = train_end)
test  <- window(log_futures, start = train_end + 1, end = test_end)

# 3. Fit ARIMA(0,1,0) model (random walk with drift)
arima_model <- Arima(train, order = c(0, 1, 0))
arima_fc <- forecast(arima_model, h = length(test))

# 4. Convert forecast back to price level
fc_prices <- exp(arima_fc$mean)
fc_xts <- xts(fc_prices, order.by = index(test))

# Only plot if 'test' and 'fc_xts' have values
if (!is.null(test) && length(test) > 0 && !any(is.na(test))) {
  png("Processed_Data/graph_8_Forecast_ARIMA.png", width = 1200, height = 800)

  plot(exp(test), main = "Futures Price: Actual vs Forecast (Drift)",
       col = "blue", lwd = 2, ylab = "Price", xlab = "Date")
  lines(fc_xts, col = "red", lwd = 2, lty = 2)
  legend("topleft", legend = c("Actual", "Forecast with Drift"),
         col = c("blue", "red"), lty = c(1, 2), lwd = 2)

  dev.off()
} else {
  warning("Test data is empty or invalid. Skipping ARIMA plot.")
}