##########################################################################################################
#                                   VAR & VECM MODEL
##########################################################################################################


#################################################################
#                  VAR AND ARIMA FORECASTING                    #
#################################################################

# 1. Load required packages
packages <- c("readr", "dplyr", "xts", "vars", "urca", "forecast")
lapply(packages[!packages %in% installed.packages()], install.packages)
invisible(lapply(packages, library, character.only = TRUE))

# 2. Load and preprocess data
data <- read_csv("Raw_Data/Coffee_Data_Set.csv") %>%
  mutate(Date = as.Date(Date))

# 3. Create log-differenced xts time series
log_diff_xts <- function(column) {
  xts_data <- xts(data[[column]], order.by = data$Date)
  na.omit(diff(log(xts_data)))
}

diff_series <- list(
  arabica = log_diff_xts("Price_Arabica"),
  robusta = log_diff_xts("Price_Robusta"),
  futures = log_diff_xts("Close_USD_60kg"),
  usd_brl = log_diff_xts("PTAX")
)

# 4. Combine series
data_ts <- na.omit(do.call(merge, diff_series))
colnames(data_ts) <- c("arabica", "robusta", "futures", "usd_brl")

# 5. Fit VAR model
lag_opt <- VARselect(data_ts, lag.max = 10, type = "const")$selection["AIC(n)"]
var_model <- VAR(data_ts, p = lag_opt, type = "const")

# 6. Forecast using VAR
h_forecast <- 14
var_fc <- predict(var_model, n.ahead = h_forecast)

# 7. Recover arabica price forecast
last_log_price <- log(tail(data$Price_Arabica, 1))
cum_log_fc <- cumsum(var_fc$fcst$arabica[,1])
forecast_log <- last_log_price + cum_log_fc
forecast_price <- exp(forecast_log)

# 8. Plot actual vs forecast
forecast_dates <- seq(max(data$Date) + 1, by = "day", length.out = h_forecast)
forecast_xts <- xts(forecast_price, order.by = forecast_dates)

arabica_xts <- xts(data$Price_Arabica, order.by = data$Date)

plot(arabica_xts, xlim = c(end(arabica_xts) - 30, max(forecast_dates)),
     main = "Arabica Spot Price: Actual and Forecast", ylab = "Price",
     col = "black", lwd = 2)
lines(forecast_xts, col = "blue", lwd = 2, lty = 2)
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "blue"),
       lty = c(1, 2), lwd = 2)

# 9. Plot individual forecasts
par(mfrow = c(3, 1))
plot(forecast_dates, var_fc$fcst$arabica[,1], type = "l", col = "blue", lwd = 2,
     main = "Forecast: Arabica", ylab = "Log Diff", xlab = "Date"); grid()
plot(forecast_dates, var_fc$fcst$robusta[,1], type = "l", col = "red", lwd = 2,
     main = "Forecast: Robusta", ylab = "Log Diff", xlab = "Date"); grid()
plot(forecast_dates, var_fc$fcst$usd_brl[,1], type = "l", col = "darkgreen", lwd = 2,
     main = "Forecast: USD/BRL", ylab = "Log Diff", xlab = "Date"); grid()
par(mfrow = c(1, 1))




#################################################################
#                       VECM FORECASTING                       #
#################################################################


# 4. Johansen cointegration test
jo_test <- ca.jo(data_ts, type = "trace", ecdet = "const", K = 2)  # Adjust K if needed
summary(jo_test)

# 5. Fit VECM and convert to VAR representation
vecm_var <- vec2var(jo_test, r = 1)  # r = number of cointegrating relationships

# 6. Forecast
h_forecast <- 14
vecm_forecast <- predict(vecm_var, n.ahead = h_forecast)

# 7. Extract forecasts
arabica_fc <- vecm_forecast$fcst$arabica[,1]
usd_brl_fc <- vecm_forecast$fcst$usd_brl[,1]

# 8. Create forecast date index
last_date <- end(data_merged)
forecast_dates <- seq(from = last_date + 1, by = "day", length.out = h_forecast)

# 9. Plot forecasts
par(mfrow = c(2, 1))

plot(forecast_dates, arabica_fc, type = "l", col = "blue", lwd = 2,
     main = "Forecast: Arabica Log Price", ylab = "Forecast", xlab = "Date")
grid()

plot(forecast_dates, usd_brl_fc, type = "l", col = "darkgreen", lwd = 2,
     main = "Forecast: USD/BRL Log Rate", ylab = "Forecast", xlab = "Date")
grid()

par(mfrow = c(1, 1))





#################################################################
#                       ARIMA FORECASTING                       #
#################################################################

# 10. ARIMA forecast (white noise model)
log_futures <- log(xts(data$Close_USD_60kg, order.by = data$Date))
train_end <- as.Date("2014-12-31")
test_end <- as.Date("2015-01-10")

train <- window(log_futures, end = train_end)
test  <- window(log_futures, start = train_end + 1, end = test_end)

arima_model <- Arima(train, order = c(0, 1, 0))
arima_fc <- forecast(arima_model, h = length(test))
fc_prices <- exp(arima_fc$mean)
fc_xts <- xts(fc_prices, order.by = index(test))

# 11. Plot ARIMA forecast
plot(exp(test), main = "Futures Price: Actual vs Forecast (Drift)",
     col = "blue", lwd = 2, ylab = "Price", xlab = "Date")
lines(fc_xts, col = "red", lwd = 2, lty = 2)
legend("topleft", legend = c("Actual", "Forecast with Drift"),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)





#####

# Set font sizes
cex_main <- 1.8   # title
cex_lab  <- 1.5   # axis labels
cex_axis <- 1.2   # axis tick labels
line_width <- 2


png("individual_forecasts.png", width = 1000, height = 900)
par(mfrow = c(3, 1))  # 3 stacked plots

plot(forecast_dates, var_fc$fcst$arabica[,1], type = "l", col = "blue", lwd = line_width,
     main = "Forecast: Arabica", ylab = "Log Diff", xlab = "Date",
     cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)
grid()

plot(forecast_dates, var_fc$fcst$robusta[,1], type = "l", col = "red", lwd = line_width,
     main = "Forecast: Robusta", ylab = "Log Diff", xlab = "Date",
     cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)
grid()

plot(forecast_dates, var_fc$fcst$usd_brl[,1], type = "l", col = "darkgreen", lwd = line_width,
     main = "Forecast: USD/BRL", ylab = "Log Diff", xlab = "Date",
     cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)
grid()

dev.off()


###
# Assume you have:
# - arabica_xts, robusta_xts, usd_brl_xts: xts objects with log-level actual data
# - var_fc: VAR forecast output (log-diff forecasts)
# - forecast_dates: future dates

# Get last known log values
last_arabica <- as.numeric(last(arabica_xts))
last_robusta <- as.numeric(last(robusta_xts))
last_usd_brl <- as.numeric(last(usd_brl_xts))

# Build cumulative log forecasts
arabica_log_fc <- last_arabica + cumsum(var_fc$fcst$arabica[, 1])
robusta_log_fc <- last_robusta + cumsum(var_fc$fcst$robusta[, 1])
usd_brl_log_fc <- last_usd_brl + cumsum(var_fc$fcst$usd_brl[, 1])

# Combine with actual data (recent 30 days)
plot_window <- 30  # days before forecast
actual_dates <- tail(index(arabica_xts), plot_window)
full_dates <- c(actual_dates, forecast_dates)

arabica_plot <- c(tail(as.numeric(arabica_xts), plot_window), arabica_log_fc)
robusta_plot <- c(tail(as.numeric(robusta_xts), plot_window), robusta_log_fc)
usd_brl_plot <- c(tail(as.numeric(usd_brl_xts), plot_window), usd_brl_log_fc)

# Plot and save
png("individual_forecasts_with_actual.png", width = 1000, height = 900)
par(mfrow = c(3, 1))

plot(full_dates, arabica_plot, type = "l", col = "black", lwd = line_width,
     main = "Arabica: Actual and Forecasted Log Prices", xlab = "Date", ylab = "Log Price",
     cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)
lines(forecast_dates, arabica_log_fc, col = "blue", lty = 2, lwd = line_width)
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "blue"),
       lty = c(1, 2), lwd = 2, cex = 1.2)
grid()

plot(full_dates, robusta_plot, type = "l", col = "black", lwd = line_width,
     main = "Robusta: Actual and Forecasted Log Prices", xlab = "Date", ylab = "Log Price",
     cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)
lines(forecast_dates, robusta_log_fc, col = "red", lty = 2, lwd = line_width)
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "red"),
       lty = c(1, 2), lwd = 2, cex = 1.2)
grid()

plot(full_dates, usd_brl_plot, type = "l", col = "black", lwd = line_width,
     main = "USD/BRL: Actual and Forecasted Log Rates", xlab = "Date", ylab = "Log Rate",
     cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)
lines(forecast_dates, usd_brl_log_fc, col = "darkgreen", lty = 2, lwd = line_width)
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "darkgreen"),
       lty = c(1, 2), lwd = 2, cex = 1.2)
grid()

dev.off()