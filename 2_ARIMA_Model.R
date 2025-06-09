##########################################################################################################
#                                   ARIMA MODEL
##########################################################################################################

# Package installation
# Load or install required packages
required_packages <- c("readr", "dplyr", "xts", "urca", "dynlm", "forecast", "portes", "tseries")

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
arabica_futures_xts <- xts(coffee_data$Close_USD_60kg, order.by = coffee_data$Date)

# 3. Merge series on common dates
data_xts <- merge(merge(arabica_spot_xts, arabica_futures_xts, join = "inner"))
colnames(data_xts) <- c("arabica_spot_price", "arabica_futures_price")

# 4. Difference of Logs
diff_log_spot_price_arabica <- na.omit(diff(log(arabica_spot_xts)))
diff_log_futures_price_arabica <- na.omit(diff(log(arabica_futures_xts)))


##########################################################################################################
###                               1. ARIMA ARABICA SPOT PRICE                                          ### 


#ACF PLOT
png("Processed_Data/graph_3_acf_plot_spot_price.png", width = 800, height = 600)
acf(diff_log_spot_price_arabica, main = "ACF Plot Spot Price Arabica")
legend("topright", legend = c("Significant Lag", "Non-significant Lag"),
       fill = c("blue", "grey"), border = NA, bty = "n")
dev.off()
#ACF don’t show significant structure I will conduct ARIMA  

arima_spot_price_log_diff_white_noise <- arima(diff_log_spot_price_arabica, order = c(0,0,0), method = "ML")
arima_spot_price_log_diff_AR <- arima(diff_log_spot_price_arabica, order = c(1,0,0), method = "ML")
arima_spot_price_log_diff_MA <- arima(diff_log_spot_price_arabica, order = c(0,0,1), method = "ML")
arima_spot_price_log_diff_ARMA <- arima(diff_log_spot_price_arabica, order = c(1,0,1), method = "ML")


summary(arima_spot_price_log_diff_white_noise)
#Only an intercept term: 0.0004
#AIC: -28409.84
#RMSE: 0.0196, MAE: 0.0146
#Residuals ACF1: -0.017

summary(arima_spot_price_log_diff_MA)
#MA1: -0.0185 (not statistically significant, s.e. = 0.0139)
#AIC: -28409.61 (slightly worse than (0,0,0))
#RMSE: 0.0196, MAE: 0.0146
#Residuals ACF1: 0.0007

summary(arima_spot_price_log_diff_ARMA)
#AR1: 0.3265, MA1: -0.3517 (both with large standard errors)
#AIC: -28409.83 (slightly better than (0,0,1), slightly worse than (0,0,0))
#RMSE: 0.0196, MAE: 0.0146
#Residuals ACF1: 0.0074

summary(arima_spot_price_log_diff_AR)


#Conclusion: Although the AIC is slightly improved over MA(1), the standard errors are large, meaning 
#parameter estimates are not precise, a white noise seems better.
##########################################################################################################
###                               2. ARIMA ARABICA FUTURE PRICE                                          ### 

#ACF PLOT
png("Processed_Data/graph_4_acf_plot_future_price.png", width = 800, height = 600)
acf(diff_log_spot_price_arabica, main = "ACF Plot Futures Price Arabica")
legend("topright", legend = c("Significant Lag", "Non-significant Lag"),
       fill = c("blue", "grey"), border = NA, bty = "n")
dev.off()
# Indicates sm

arima_future_price_log_diff_white_noise  <- arima(diff_log_futures_price_arabica, order = c(0,0,0), method = "ML")
arima_future_price_log_diff_AR <- arima(diff_log_futures_price_arabica, order = c(1,0,0), method = "ML")
arima_future_price_log_diff_MA  <- arima(diff_log_futures_price_arabica, order = c(0,0,1), method = "ML")
arima_future_price_log_diff_ARMA <- arima(diff_log_futures_price_arabica, order = c(1,0,1), method = "ML")

summary(arima_future_price_log_diff_white_noise)
#Intercept: 0.0004
#AIC: -27440.27 -> lowest AIC among all
#RMSE: 0.02138
#MAE: 0.01596
#Residual ACF1: -0.0036

summary(arima_future_price_log_diff_MA)
#MA1: -0.0037 (very close to zero, and not significant, s.e. = 0.0136)
#AIC: -27438.35 (worse than white noise)
#Error metrics: Nearly identical to ARIMA(0,0,0)
#Residual ACF1: ~0

summary(arima_future_price_log_diff_ARMA)
# AR1 & MA1: Both near zero and not estimated properly (s.e. = NaN → parameters not identifiable)
#AIC: -27436.34 (worst of all three models)
#RMSE/MAE: Identical to others
#Residual ACF1: ~0

summary(arima_future_price_log_diff_AR) 

#Adding a MA(1) term does not improve the model, and might be unnecessary complexity. NaNs in standard 
#errors suggest model overfitting / collinearity / numerical issues in the ARMA model.
#Conclusion: Once again the white noise model fits well and has the lowest AIC, meaning it's the most 
#parsimonious with decent predictive accuracy.

##########################################################################################################
###                               3.LJUNG-BOX TEST                                                     ### 


#Since the ARIMA(0,0,0) basically assumes white noise, the Ljung-Box test will confirm if that assumption 
#holds—i.e., residuals are uncorrelated. 

dates <- coffee_data$Date  # e.g., a Date class vector of length 5400 I did this since I wanted to
# see when the spikes occured.

# Align dates with residual length
dates_spot <- tail(dates, length(arima_spot_price_log_diff_white_noise$residuals))
dates_future <- tail(dates, length(arima_future_price_log_diff_white_noise$residuals))

png("Processed_Data/graph_5_Residual_Plots.png", width = 800, height = 600)
par(mfrow = c(2, 4))  # 2x4 layout

# Plot Spot residuals vs dates
plot(dates_spot, arima_spot_price_log_diff_white_noise$residuals,
     type = "l",
     main = "Spot (0,0,0) Residual",
     ylab = "Residual",
     xlab = "Date")
abline(h = mean(arima_spot_price_log_diff_white_noise$residuals),
       lty = "dashed",
       lwd = 2,
       col = "blue")

# Plot Future residuals vs dates
plot(dates_future, arima_future_price_log_diff_white_noise$residuals,
     type = "l",
     main = "Future (0,0,0) Residual",
     ylab = "Residual",
     xlab = "Date")
abline(h = mean(arima_future_price_log_diff_white_noise$residuals),
       lty = "dashed",
       lwd = 2,
       col = "blue")

acf(arima_spot_price_log_diff_white_noise$residuals,
    main = "ACF - Spot Residuals")

acf(arima_future_price_log_diff_white_noise$residuals,
    main = "ACF - Future Residuals")

hist(arima_spot_price_log_diff_white_noise$residuals,
     breaks = 50,
     main = "Histogram of Spot Price Residuals",
     xlab = "Residual",
     col = "lightgray",
     border = "white")
abline(v = mean(arima_spot_price_log_diff_white_noise$residuals), col = "blue", lwd = 2, lty = 2)

hist(arima_future_price_log_diff_white_noise$residuals,
     breaks = 50,
     main = "Histogram of Futures Price Residuals",
     xlab = "Residual",
     col = "lightgray",
     border = "white")
abline(v = mean(arima_future_price_log_diff_white_noise$residuals), col = "blue", lwd = 2, lty = 2)

qqnorm(arima_future_price_log_diff_white_noise$residuals,
       main = "Q-Q Plot of Spot Price Residuals")
qqline(arima_future_price_log_diff_white_noise$residuals, col = "blue", lwd = 2)

qqnorm(arima_future_price_log_diff_white_noise$residuals,
       main = "Q-Q Plot of Future Price Residuals")
qqline(arima_future_price_log_diff_white_noise$residuals, col = "blue", lwd = 2)

dev.off()

####

# Compute q once
q <- floor(0.75 * length(arima_spot_price_log_diff_white_noise$residuals)^(1/3))

# Count number of model coefficients
ncoeff_spot <- length(coef(arima_spot_price_log_diff_white_noise))
ncoeff_future <- length(coef(arima_future_price_log_diff_white_noise))

# Ljung-Box tests using same q
Box.test(arima_spot_price_log_diff_white_noise$residuals, lag = q, type = "Ljung-Box", fitdf = ncoeff_spot)
#X-squared = 26.97
#df = 12
#p-value = 0.0078

Box.test(arima_future_price_log_diff_white_noise$residuals, lag = q, type = "Ljung-Box", fitdf = ncoeff_future)
#X-squared = 20.15
#df = 12
#p-value = 0.0643


LjungBox(arima_spot_price_log_diff_white_noise$residuals, lags = q, fitdf = ncoeff_spot)
LjungBox(arima_future_price_log_diff_white_noise$residuals, lags = q, fitdf = ncoeff_future)

IC <- data.frame(
  Model = c("Spot(0,0,0)", "Future(0,0,0)"),
  AIC = c(AIC(arima_spot_price_log_diff_white_noise), AIC(arima_future_price_log_diff_white_noise)),
  BIC = c(BIC(arima_spot_price_log_diff_white_noise), BIC(arima_future_price_log_diff_white_noise))
)

# IC
#          Model       AIC       BIC
#1   Spot(0,0,0) -28409.84 -28396.56
#2 Future(0,0,0) -27440.27 -27426.99
#Spot (0,0,0)	       -28409.84	0.0196	       0.0146        0.0078 (significant autocorrelation)
#Futures (0,0,0)	-27440.27	0.02138	0.01596	0.0643 (borderline, no strong autocorrelation)

# Despite the AIC value for the spot model being higher, the signifcant autocorrelation has led me to use
# the future one instead. However since log returns are noise, future returns can’t be predicted beyond the average.


#High volatility: Prices swing a lot due to weather shocks, pests, political issues in producing countries, and speculation.
#External factors: Coffee prices depend on many unpredictable variables like climate conditions, currency exchange rates, global demand, and supply disruptions.
#Efficient markets: Commodity markets often quickly incorporate available information, so future price movements are close to a “random walk” — hard to forecast better than chance.
#Noise-like behavior: As you saw in your ARIMA analysis, the log returns (price changes) often behave like white noise, meaning they have little autocorrelation and no clear patterns.