
##########################################################################################################
#                                   ARIMA MODEL
##########################################################################################################

# Package installation
# Load or install required packages
required_packages <- c("readr", "dplyr", "xts", "urca", "dynlm", "forecast")

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
data_xts <- merge(merge(arabica_spot_xts, robusta_spot_xts, join = "inner"))
colnames(data_xts) <- c("arabica_spot_price", "arabica_futures_price")

# 4. Difference of Logs
diff_log_spot_price_arabica <- na.omit(diff(log_spot_price_arabica))
diff_log_futures_price_arabica <- na.omit(diff(log_futures_price_arabica))



png("Processed_Data/acf_plot_spot_price.png", width = 800, height = 600)
acf(diff_log_spot_price_arabica, main = "ACF Plot Spot Price Arabica")
legend("topright", legend = c("Significant Lag", "Non-significant Lag"),
       fill = c("blue", "grey"), border = NA, bty = "n")
dev.off()

png("Processed_Data/acf_plot_future_price.png", width = 800, height = 600)
acf(diff_log_spot_price_arabica, main = "ACF Plot Futures Price Arabica")
legend("topright", legend = c("Significant Lag", "Non-significant Lag"),
       fill = c("blue", "grey"), border = NA, bty = "n")
dev.off()

#ACF don’t show significant structure I will conduct ARIMA model using


arima_spot_price_log_diff <- arima(diff_log_spot_price_arabica, order = c(0,0,0), method = "ML")
arima_future_price_log_diff  <- arima(diff_log_futures_price_arabica, order = c(0,0,0), method = "ML")

summary(arima_spot)
summary(arima_futures)

#Since the ARIMA(0,0,0) basically assumes white noise, the Ljung-Box test will confirm if that assumption holds—i.e., residuals are uncorrelated.
# If the test shows no significant autocorrelation, you’re good. If it does, you may want to consider more complex AR or MA terms.


##############3

par(mfrow=c(2,2)) # to plot the four plots below in a 2 x 2 grid
plot(arima_spot$residuals,
     main = "Spot Residual",
     ylab = "Residual",
     xlab = "",
)
abline(h = mean(arima_spot$residuals),
       lty = "dashed",
       lwd = 2,
       col = "blue")

plot(arima_futures$residuals,
     main = "ARMA(1,1) Residual",
     ylab = "Residual",
     xlab = "",
)
abline(h = mean(arima_futures$residuals),
       lty = "dashed",
       lwd = 2,
       col = "blue")

Acf(arima_spot$residuals, 
    main = "MA(1) Residual",
    ylab = "ACF",
    xlab = "")

Acf(arima_futures$residuals, 
    main = "ARMA(1,1) Residual",
    ylab = "ACF",
    xlab = "")

# It is not unexpected that these plots look similar for both series (why?) but both look 
# good!












library(forecast)
auto.arima(diff_log_spot_price_arabica, d=1)
auto.arima(diff_log_futures_price_arabica, d=1)


Acf(arima_spot$residuals)
Acf(arima_futures$residuals)

Box.test(arima_spot$residuals, lag = q, type = "Ljung-Box", fitdf = length(coef(arima_spot)))
Box.test(arima_futures$residuals, lag = q, type = "Ljung-Box", fitdf = length(coef(arima_futures)))






data.frame(
  Model = c("Spot", "Futures"),
  AIC = c(AIC(arima_spot), AIC(arima_futures)),
  BIC = c(BIC(arima_spot), BIC(arima_futures))
)






############################################################################################

# We now want to "identify, estimate and do diagnostics of at least one ARIMA-model". We know 
# from the tests above that we want to estimate an ARIMA(p,1,q) for some p and q, i.e. an 
# ARMA(p,q) model for the difference of the real M1 money stock.

# Plotting the estimated autocorrelation function

Acf(dm)

# An ARMA(1,1) or MA(1) seems like good candidates (why?). Let's first use the arima() function
# to estimate and check coefficients.

MA1 <- arima(dm, order = c(0,0,1), method = "ML")
ARMA11 <- arima(dm, order = c(1,0,1), method = "ML")

summary(MA1)
summary(ARMA11)

############################################################################################

# Ljung-Box test

# Let's start by plotting the residuals and check the ACF of both models

par(mfrow=c(2,2)) # to plot the four plots below in a 2 x 2 grid
plot(MA1$residuals,
     main = "MA(1) Residual",
     ylab = "Residual",
     xlab = "",
)
abline(h = mean(MA1$residuals),
       lty = "dashed",
       lwd = 2,
       col = "blue")

plot(ARMA11$residuals,
     main = "ARMA(1,1) Residual",
     ylab = "Residual",
     xlab = "",
)
abline(h = mean(ARMA11$residuals),
       lty = "dashed",
       lwd = 2,
       col = "blue")

Acf(MA1$residuals, 
    main = "MA(1) Residual",
    ylab = "ACF",
    xlab = "")

Acf(ARMA11$residuals, 
    main = "ARMA(1,1) Residual",
    ylab = "ACF",
    xlab = "")

# It is not unexpected that these plots look similar for both series (why?) but both look 
# good!

# We now perform Ljung-Box tests. First we compute the appropriate number of lags to choose 
# using the the usual rule of thumb.

q <- floor(0.75*nobs(MA1)^(1/3)) # = 4

# Then compute the number of parameters for our two models.

ncoeff_MA1 <- length(coef(MA1)) # = 2
ncoeff_ARMA11 <- length(coef(ARMA11)) # = 3

# Then use the LjungBox function to perform the LjungBox test. Remember that the null 
# hypothesis of the Ljung-Box test is autocorrelation = 0 for lags 1,...,q.

LjungBox(MA1$residuals, lags = q, fitdf = ncoeff_MA1)
LjungBox(ARMA11$residuals, lags = q, fitdf = ncoeff_ARMA11)

# Can not reject the null in both cases which does not surprise me given the plot.

# Finally we want to rank our two models using AIC and BIC (Bayesian information
# criterion). Recall that:

# AIC: 2k - 2log(L),
# BIC: log(n)k - 2log(L),

# and remember that lower is better!

IC <- data.frame(
  Model = c("MA(1)", "ARMA(1,1)"),
  AIC = c(AIC(MA1), AIC(ARMA11)),
  BIC = c(BIC(MA1), BIC(ARMA11))
)

IC
