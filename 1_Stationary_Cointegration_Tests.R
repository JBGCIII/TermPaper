##########################################################################################################
#                                   PRE-VAR ANALYSIS 
##########################################################################################################


##########################################################################################################
###                               1. Arabica and Robusta Prices                                        ### 
##########################################################################################################




# Load libraries
library(readr)
library(dplyr)
library(xts)     # Note, I was forced to use xts instead of ts as ts does not account for irregular data.
library(urca)    # For ur.df() and ur.kpss()
library(dynlm)   # For dynlm()

# Read CSV and preprocess
coffee_data <- read_csv("Raw_Data/Coffee_Data_Set.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  arrange(Date)

# Create xts objects indexed by Date
arabica_spot_xts <- xts(coffee_data$Price_Arabica, order.by = coffee_data$Date)
arabica_futures_xts <- xts(coffee_data$Close_USD_60kg, order.by = coffee_data$Date)

# Merge and keep only dates with both series
data_xts <- merge(arabica_spot_xts, arabica_futures_xts, join = "inner")
colnames(data_xts) <- c("arabica_spot_price", "arabica_futures_price")

# Extract numeric vectors for tests
spot_vec <- coredata(data_xts$arabica_spot_price)
futures_vec <- coredata(data_xts$arabica_futures_price)

# Unit root tests on levels
summary(ur.df(spot_vec, type = "none", selectlags = 45))    # ADF on spot price, no trend
summary(ur.df(spot_vec, type = "drift", selectlags = "AIC"))   # ADF on spot price, with drift
summary(ur.df(spot_vec, type = "trend", selectlags = "AIC"))   # ADF on spot price, with trend

summary(ur.df(futures_vec, type = "none", selectlags = "AIC"))    # ADF on futures price, no trend
summary(ur.df(futures_vec, type = "drift", selectlags = "AIC"))   # ADF on futures price, with drift
summary(ur.df(futures_vec, type = "trend", selectlags = "AIC"))   # ADF on futures price, with trend

summary(ur.kpss(spot_vec))       # KPSS test on spot price
summary(ur.kpss(futures_vec))    # KPSS test on futures price

# Log transform
log_spot <- log(spot_vec)
log_futures <- log(futures_vec)

# Unit root tests on log-levels
summary(ur.df(log_spot, type = "none", selectlags = "AIC"))
summary(ur.df(log_spot, type = "drift", selectlags = "AIC"))
summary(ur.df(log_spot, type = "trend", selectlags = "AIC"))

summary(ur.df(log_futures, type = "none", selectlags = "AIC"))
summary(ur.df(log_futures, type = "drift", selectlags = "AIC"))
summary(ur.df(log_futures, type = "trend", selectlags = "AIC"))


summary(ur.kpss(spot_vec))       # KPSS test on spot price
summary(ur.kpss(futures_vec))    # KPSS test on futures price
summary(ur.kpss(log_spot))
summary(ur.kpss(log_futures))
summary(ur.kpss(diff_log_spot))
summary(ur.kpss(diff_log_futures))


# First differences of log prices
diff_log_spot <- diff(log_spot)
diff_log_futures <- diff(log_futures)

# Unit root tests on differenced logs
summary(ur.df(diff_log_spot, type = "none", selectlags = "AIC"))
summary(ur.df(diff_log_spot, type = "drift", selectlags = "AIC"))
summary(ur.df(diff_log_spot, type = "trend", selectlags = "AIC"))

summary(ur.df(diff_log_futures, type = "none", selectlags = "AIC"))
summary(ur.df(diff_log_futures, type = "drift", selectlags = "AIC"))
summary(ur.df(diff_log_futures, type = "trend", selectlags = "AIC"))

summary(ur.kpss(diff_log_spot))
summary(ur.kpss(diff_log_futures))








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




################################################Classic Dickey-Fuller test###########################################

# Load required packages
library(readr)
library(urca)

# Load the logged data
log_log_returns_data <- read_csv("Processed_Data/Log_and_Log_Returns_Data.csv")

# ADF test on logged data
arabica_series <- log_log_returns_data$log_Price_Arabica 
robusta_series <- log_log_returns_data$log_Price_Robusta 
exchange_rate_series <- log_log_returns_data$log_PTAX 
coffe_futures_series <- log_log_returns_data$log_Close_USD_60kg

# Run ADF test with no constant or trend, 0 lag.
DF_arabica <- ur.df(arabica_series, type = "none", lags = 0)
DF_robusta <- ur.df(robusta_series, type = "none", lags = 0)
DF_Exhange_Rate <- ur.df(exchange_rate_series, type = "none", lags = 0)
DF_Coffe_Futures <- ur.df(coffe_futures_series, type = "none", lags = 0)

# Show summary
summary(DF_arabica) # 1.3987 > -1.95, fail to reject the null hypothesis.
summary(DF_robusta) #1.8959 > -1.95, fail to reject the null hypothesis.
summary(DF_Exhange_Rate)# 0.9163 > -1.95, fail to reject the null hypothesis.
summary(DF_Coffe_Futures) # 1.1233 > -1.95, fail to reject the null hypothesis.

# The p-value indicates that none of the series are stationary at conventional significance levels 
# Instead of using lags for daily data which would require a large ammount of lags I decided to calculate
# log retuns instead

# ADF test on log returns
log_return_arabica_series <- log_log_returns_data$ret_Arabica 
log_return_robusta_series <- log_log_returns_data$ret_Close_USD_60kg 
log_return_exchange_rate_series <- log_log_returns_data$ret_PTAX 
log_return_coffe_futures_series <- log_log_returns_data$ret_Close_USD_60kg

# Run ADF test with no constant or trend, 0 lag.
DF_log_return_arabica <- ur.df(log_return_arabica_series, type = "none", lags = 0)
DF_log_return_robusta <- ur.df(log_return_robusta_series, type = "none", lags = 0)
DF_log_return_Exhange_Rate <- ur.df(log_return_exchange_rate_series, type = "none", lags = 0)
DF_log_return_Coffe_Futures <- ur.df(log_return_coffe_futures_series, type = "none", lags = 0)

# Show summary
summary(DF_log_return_arabica) # -76.43 < -1.95,  strongly reject the null hypothesis.
summary(DF_log_return_robusta) #−75.44 < -1.95, strongly reject the null hypothesis.
summary(DF_log_return_exhange_rate)# -70.5777 < -1.95, strongly reject the null hypothesis.
summary(DF_log_return_coffe_futures) # -70.5777 < -1.95, strongly reject the null hypothesis.


#################################Kwiatkowski–Phillips–Schmidt–Shin (KPSS) #########################################

# KPSS test on original logged prices
summary(ur.kpss(arabica_series)) #19.1882 > 0.739, Reject null hypothesis.
summary(ur.kpss(robusta_series)) #21.7384 > 0.739, Reject null hypothesis. 
summary(ur.kpss(exchange_rate_series)) #34.1419 > 0.739, Reject null hypothesis.
summary(ur.kpss(coffe_futures_series)) #20.1737 > 0.739, Reject null hypothesis.

# KPSS test on log returns
summary(ur.kpss(log_return_arabica_series)) #0.1311 < 0.739, Do Not Reject null hypothesis.
summary(ur.kpss(log_return_robusta_series)) #0.084 < 0.739, Reject Not Reject null hypothesis.
summary(ur.kpss(log_return_exchange_rate_series)) #0.2443 < 0.739, Do Not Reject null hypothesis.
summary(ur.kpss(log_return_coffe_futures_series)) #0.084 < 0.739, Do Not Reject null hypothesis.












# Augmented Dickey–Fuller test (ADF) test

# We first do a classic Dickey-Fuller test using the ur.df() function

DF_m <- ur.df(m, type = "none", lags = 0) 
summary(DF_m)

# The p-value equals 0.027 so we reject the null hypothesis of a unit root at the 5% 
# significance level.

# We now want to do an ADF test but which AR(p) model should we test? The ur.df function has 
# the option selectlags which together with the lags option will choose the order using the
# specified information criteria on estimated AR(1),AR(2),... up to the specified number of
# lags. Here we will choose AIC and the rule of thumb is to choose lags so that the number 
# of lags = 3 years in the frequency of the series (that is lags = 3 for yearly, lags = 12 
# for quarterly, lags = 36 for monthly).

ADF_m <- ur.df(m, type = "none", selectlags = "AIC", lags = 12)
ADF_drift_m <- ur.df(m, type = "drift", selectlags = "AIC", lags = 12) # + intercept
ADF_trend_m <- ur.df(m, type = "trend", selectlags = "AIC", lags = 12) # + intercept & trend

summary(ADF_m)
summary(ADF_drift_m)
summary(ADF_trend_m)

# Note that if we did not include selectlags = "AIC", then

# 1 differenced lag = AR(2) chosen
# 2 differenced lag = AR(3) chosen
# 3 differenced lag = AR(4) chosen etc.

# Now since none of the tests can reject the null of a unit root we difference the data and
# plot it.

dm <- diff(m) # note that dm is the % change in the real M1 money stock

plot(dm,
     main = "% Change Real Money Stock (M1)",
     ylab = "% Change",
     xlab = ""
)
abline(h = mean(dm),
       lty = "dashed",
       lwd = 2,
       col ="blue")

# Looks stationary but we should do the ADF-tests again

ADF_dm <- ur.df(dm, type = "none", selectlags = "AIC", lags = 12)
ADF_drift_dm <- ur.df(dm, type = "drift", selectlags = "AIC", lags = 12)
ADF_trend_dm <- ur.df(dm, type = "trend", selectlags = "AIC", lags = 12)

summary(ADF_dm)
summary(ADF_drift_dm)
summary(ADF_trend_dm)

# All three tests reject the null of a unit root

############################################################################################

# Kwiatkowski–Phillips–Schmidt–Shin (KPSS) test

# The KPSS test is another unit root test. The null of the KPSS test is that the series is 
# trend stationary and the alternative hypothesis is the existence of a unit root. The 
# ur.kpss() function performs a KPSS test.

summary(ur.kpss(m))
summary(ur.kpss(dm))

# Note that the larger the test statistic, the lower the p-value; i.e. we "want" a small test 
# statistic to not reject the null hypothesis of trend stationarity.













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
