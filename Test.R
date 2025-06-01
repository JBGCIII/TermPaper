

# Now I'm going to load a dataset from the excel file swedish_gdp.xlsx that I downloaded from 
# SCB. The data is seasonally adjusted quarterly Swedish GDP from 1993:Q1 to 2024:Q4 in 
# millions of SEK. To read excel files we need the 'readxl' package.

install.packages("readxl")
library(readxl)

# The read_excel() function saves the specified cells as a data frame. The first entry of 
# each column is treated as the name of the column unless you include col_names = FALSE.
# x <- y means create object x defined as y.

GDP <- read_excel("swedish_gdp.xlsx", range = "B52:B179", col_names = FALSE)

# The ts() function turns an object into a time series object starting at some date with 
# some frequency. The code below takes the data frame GDP and turns it into a time series 
# starting at 1993 with a frequency of 4. I end with /1000 so that the data is in billions
# rather than millions of SEK. NB! Everything I'm doing here requires knowledge about the 
# raw data.

GDP <- ts(GDP, start = 1993, frequency = 4)/1000

# The plot() function is an example of a command that would not work (well) if we didn't 
# first convert the data into a time series object.

plot(GDP, 
     main = "Swedish Quarterly GDP",
     ylab = "SEK billions", 
     xlab = "", 
     lwd = 2)

############################################################################################

# There is a clear upward trend in GDP so let's try a few ways to detrend it.
# Simplest way is to use the diff() function with differences = 1 to take first differences.

GDP_delta <- diff(GDP, differences = 1)

# Note that we lost the first observation in our time series (1993:Q1). Plot looks 
# stationary. The number of differences required to obtain a stationary series is called the 
# order of integration of the series.

plot(GDP_delta,
     main = "Change in Swedish Quarterly GDP",
     ylab = "SEK billions", 
     xlab = "",
     lwd = 2)

############################################################################################

# Another simple way to detrend is to compute the % change since last year (i.e. four 
# quarters ago). The 'tfplot' package includes the annualizedGrowth() function.

install.packages("tfplot")
library(tfplot)

# annualizedGrowth() with lag = frequency of your series gives you the annual growth rate.

GDP_growth <- annualizedGrowth(GDP, lag = 4)

# Note that we now lost the first four observations in our time series (1993:Q1-Q4). Also 
# looks stationary.

plot(GDP_growth,
     main = "% change in Swedish Quarterly GDP since last year",
     ylab = "% change", 
     xlab = "",
     lwd = 2)

############################################################################################

# The 'mFilter' package includes the hpfilter() function which applies the HP filter to a 
# time series object. Remember that there is some debate about the HP filter and that there
# is no universally "correct" detrending procedure.

install.packages("mFilter")
library(mFilter)

# Note that hpfilter() sets the smoothness parameter lambda to 1600 by default (the most 
# common choice for quarterly data). To choose a different lambda we would specify 
# freq = lambda, e.g.  hpfilter(GDP, freq = 800). NB! Many functions such as hpfilter() 
# has default options that you need to be aware of, i.e. know what your functions do.

GDP_hp <- hpfilter(GDP)

# GDP_hp is a list of 10 objects. In the next plot I need to specify that it is the object 
# cycle (a time series) in the list GDP_hp that I want to plot.

plot(GDP_hp$cycle, 
     main = "Swedish Quarterly GDP (HP cycle w/ lambda = 1600)",
     ylab = "SEK billions", 
     xlab = "", 
     lwd = 2)







# Econometrics 3b - Spring 2025
# Lab 2
# Fredrik Runelöv (fredrik.runelov@su.se)

############################################################################################

# As always we start by loading all the packages we need. Type e.g. install.packages("dynlm") 
# in the console below to install.

library(dynlm) # to estimate AR(p) by OLS
library(forecast) # to estimate and plot the ACF
library(portes) # to perform Ljung-Box test
library(tseries) # to perform Jarque-Bera test

# Let's load the Macrodat dataset without loading the full Ecdat package since we only need 
# the dataset.

data(Macrodat, package = "Ecdat")

# The model for gdpjp (= real GDP for Japan) we want to estimate is an AR(1) process in 
# first-differences.

formula1 <- d(gdpjp) ~ L(d(gdpjp)) # d and L are internal dynlm functions

# Use dynlm from the dynlm package to estimate with OLS.

model1 <- dynlm(formula1, Macrodat)

# model1 is a list containing all the results of the regression and we can use the summary() 
# function from the base package to summarize the results.

summary(model1)

# NB! You do not want to use the lm() function for regressions with differences and lags. You 
# can do it but it's more complicated.

############################################################################################

# Now let's plot the residuals. I also save the plot in a pdf since that is the format Latex 
# likes the best.

pdf("residuals.pdf") # "open" a pdf-file
plot(model1$residuals,
     main = "Model 1",
     ylab = "Residual", 
     xlab = "", 
     col = "blue"
     )
abline(h = 0,
       col = "red",
       lty = "dashed"
       )
dev.off() # "close" and save file

# Appears to be some heteroskedasticity and autocorrelation. The Acf() function from the 
# forecast package plots the sample autocorrelation function.

Acf(model1$residuals)

############################################################################################

# Appears to be some correlation so let's do a Ljung-Box test. The null hypothesis of the 
# Ljung-Box test is that the population autocovariance function = 0 for lags h = 1,2,...,q but
# what is an appropriate q? q = 0.75*n^(1/3) rounded down to the nearest integer is a common 
# rule of thumb. We also need to specify the number of estimated parameters. The function 
# LjungBox from the portes package can be used to perform the Ljung-Box test. However it might
# be the case that the q obtained by the rule of thumb is less than or equal to the number of 
# estimated parameters and then we need to choose q = 0.75*n^(1/3) + the number of estimated 
# parameters. For example below we obtain q = 4 and have 2 estimated parameters but suppose we
# instead estimated an AR(3) model, then we would have had 4 estimated parameters and should 
# have chosen q = 4 + 4 instead.

q <- floor(0.75*nobs(model1)^(1/3)) # the floor function rounds down to an integer
param = length(model1$coefficients)
LjungBox(model1$residuals, lags = q, fitdf = param)

############################################################################################

# As a bonus we perform a Jarque-Bera test. The null hypothesis of the Jarque-Bera test is that
# the true distribution of the data is a normal distribution. The function jarque.bera.test() 
# from the tseries package can be used to perform a Jarque-Bera test.

hist(model1$residuals, # first take a look at the histogram
     probability = TRUE,
     main = "Model 1 Residuals",
     xlab ="")

jarque.bera.test(model1$residuals)

############################################################################################

# We now estimate an AR(1), AR(2), AR(3) and AR(4) process for the change in gdpjp. To make 
# sure that each model is estimated using the same number of observations we use the embed 
# command with dimension = 1 + 4.

d_gdpjp <- diff(Macrodat[,"gdpjp"])
d_gdpjp1 <- embed(d_gdpjp, dimension = 1 + 4)

# The embed command turns d_gdpjp from a time series to a matrix. To turn it back into a time 
# series we use the ts() function.

d_gdpjp1 <- ts(d_gdpjp1, start = c(1960,2), frequency = 4)

# we can now use the dynlm() function to estimate each AR() model by OLS. The original series 
# is regressed on the series "to the right".

AR1 <- dynlm(d_gdpjp1[,1] ~ d_gdpjp1[,2:2])
AR2 <- dynlm(d_gdpjp1[,1] ~ d_gdpjp1[,2:3])
AR3 <- dynlm(d_gdpjp1[,1] ~ d_gdpjp1[,2:4])
AR4 <- dynlm(d_gdpjp1[,1] ~ d_gdpjp1[,2:5])

# Summaries

summary(AR1)
summary(AR2)
summary(AR3)
summary(AR4)

# Note that AR4 is equivalent to:

formula2 <- d(gdpjp) ~ L(d(gdpjp)) + L(d(gdpjp),2) + L(d(gdpjp),3) + L(d(gdpjp),4)
model2 <- dynlm(formula2, Macrodat)
summary(model2)


############################################################################################

# Econometrics 3b - Spring 2025
# Lab 3
# Fredrik Runelöv (fredrik.runelov@su.se)

############################################################################################

# Load libraries

library(forecast) # Acf(), auto.arima()
library(urca) # ur.df(), ur.kpss()
library(portes) # LjungBox()

# Load the data

data(MoneyUS, package = "Ecdat") # US macro data from 1959:Q1 to 2000:Q4
m <- MoneyUS[,"m"] # m = log of real M1 money stock

# Plot the data

plot(m,
     main = "Log of Real Money Stock (M1)",
     ylab = "Log M1",
     xlab = "",
)
abline(h = mean(m),
       lty = "dashed",
       lwd = 2,
       col = "blue")

# This does not look stationary!

Acf(m)

############################################################################################

# First we use auto.arima()

ARIMA <- auto.arima(m)

# Summary

summary(ARIMA)

# auto.arima suggests an ARIMA(0,1,1)

############################################################################################

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

############################################################################################

# A final note about the auto.arima() function. If we do not include the arguments 
# stepwise = FALSE and approximation = FALSE, then auto.arima() takes some shortcuts so you 
# should include them if your estimating say a single ARIMA model on a single time series.

ARIMA <- auto.arima(m, stepwise = FALSE, approximation = FALSE) 

# Note that it took a little bit of time though

summary(ARIMA)


############################################################################################

# Econometrics 3b - Spring 2025
# Lab 4
# Fredrik Runelöv (fredrik.runelov@su.se)

############################################################################################

# Load libraries

library(forecast) # auto.arima(), forecast(), dm.test()
library(dynlm) # dynlm()
library(lmtest) # coeftest()
library(sandwich) # NeweyWest()

# Load data

data(MoneyUS, package = "Ecdat") # US macro data from 1959:Q1 to 2000:Q4
infl <- MoneyUS[,"infl"] # annual inflation rate

rm(MoneyUS) # can remove MoneyUS from the global environment as we won't use it anymore

# Plot data

plot(infl,
     main = "Annual US Inflation Rate",
     ylab = "Inflation Rate",
     xlab = "",
     lwd = 2)

############################################################################################

# First let's use an ARIMA model to make a forecast at the single forecast origin 1989:Q4

# The function auto.arima() selects and estimates the ARIMA model that best fits the data

example_m <- auto.arima(window(infl, end = 1989 + 3/4))

# The window() function selects a subset of the time series with options start and end

# R stores quarterly time indices as 1985, 1985.25, 1985.50, 1985.75, i.e. in steps of 1/4
# and monthly time indices as 1980, 1980 + 1/12, 1980 + 2/12,... etc., i.e. in steps of 1/12.

summary(example_m) # note that auto.arima() chose an ARIMA(1,1,1) model

# We now use the forecast() function to use the ARIMA model that auto.arima() chose to make
# forecasts for horizons h = 1,...,4.

example_f <- forecast(example_m, h = 4)

# The time series mean in the list produced by the forecast() function contains forecasted 
# point estimates. 

# Let's plot the actual series together with our forecast

ts.plot(window(infl, start = 1987, end = 1993),example_f$mean,
        main = "Annual US Inflation Rate",
        ylab = "Inflation Rate",
        xlab = "",
        lty = c("solid", "dotted"),
        lwd = c("2", "2"),
        col = c("blue", "red"))
legend("topleft",
       legend = c("Inflation Rate", "Forecast"),
       lty = c("solid", "dotted"),
       lwd = c("2", "2"),
       col = c("blue", "red"))

############################################################################################

# We have 40 quarterly observations between 1985:Q1 and 1994:Q4. We want to forecast for the 
# horizons h = 1,2,3,4 so we have 36 forecast origins.

hmax <- 4 # the maximum forecast horizon we're gonna use
origin_1 <- 1985 # first forecast origin
n_origins <- length(window(infl, start = origin_1)) - hmax # total number of forecast origins

# We start by creating two lists of length 36 in which we will store our models and forecasts

ARIMA_models <- vector("list", n_origins)
ARIMA_forecasts <- vector("list", n_origins)

# We then create four time series starting at the first forecast origin. We will store our 
# forecasts for horizons h = 1,2,3,4 in these series.

ARIMA_results <- ts(matrix(NA, nrow = n_origins, ncol = hmax), 
              start = origin_1,
              frequency = frequency(infl))

# We can use the paste0() function to change the column names

colnames(ARIMA_results) <- paste0("h = ", 1:hmax)

# We now do loop from 1 to 36 and do the following at each step:

# 1) Define the origin which starts at 1985 and adds 1/4 at each step
# 2) Subset the data with the window() function
# 3) Use the auto.arima function on the subset of data in 2)
# 4) Use the forecast() function with the ARIMA model from 3) to forecast horizons = 1,2,3,4
# 5) Save model
# 6) Save forecast
# 7) Save results

# The commented part shows the necessary replacements to do a rolling forecast instead

for (i in 1:n_origins) {
  # ro_start <- start(infl) + (i - 1)/4
  origin <- origin_1 + (i - 1)/4
  data <- window(infl, end = origin) # data <- window(infl, start = ro_start, end = origin)
  m <- auto.arima(data)
  f <- forecast(m, h = hmax)
  ARIMA_models[[i]] <- m
  ARIMA_forecasts[[i]] <- f
  ARIMA_results[i,] <- f$mean
}
rm(i,origin,data,m,f) # the loop will leave a bunch of objects in the global environment
                      # that we don't want to keep

# Note that we could replace auto.arima(data) in the loop with 
# arima(data, order = c(p,d,q), method = "ML") to estimate a given ARIMA model with maximum 
# likelihood at each forecast origin. For example arima(data, order = c(1,0,0), method = "ML") 
# would estimate an AR(1) model at each forecast origin and 
# arima(data, order = c(1,1,1), method = "ML") would estimate an ARIMA(1,1,1) model at each
# forecast origin.

############################################################################################

# The lapply function applies a local function over a list or vector and stores
# the result in a temporary list.

temp <- lapply(ARIMA_forecasts, function(x) x$mean)

# temp is a list of 36 objects and each object is a time series of length four.

# Now we use the do.call() function to apply the ts.union() function to the objects in temp. 
# The ts.union() takes two time series of the same frequency and combines them, adding NAs 
# as necessary.

ARIMA_results_plot <-  do.call(ts.union,temp)
rm(temp) # no need for the list anymore

# ARIMA_results_plot is now a multivariate time series from 1985:Q2-1994:Q4

ts.plot(ARIMA_results_plot,
  main = "Recursive ARIMA forecasts",
  ylab = "Inflation Rate",
  xlab = "",
  lty = "dotted",
  lwd = "2",
  col = "red")
lines(window(infl, start = origin_1),
      lwd = "2",
      col = "blue")
legend("topleft",
       legend = c("Inflation rate", "Forecast"),
       lty = c("solid", "dotted"),
       lwd = c("2", "2"),
       col = c("blue", "red")
)

############################################################################################

# In this section I do the exact same thing as in the above section except now for a fixed 
# AR(1) model for the first difference.

AR1_models <- vector("list", n_origins)
AR1_forecasts <- vector("list", n_origins)

AR1_results <- ts(matrix(NA, nrow = n_origins, ncol = hmax), 
              start = origin_1,
              frequency = frequency(infl))

colnames(AR1_results) <- paste0("h = ", 1:hmax)

for (i in 1:n_origins) {
  origin <- origin_1 + (i - 1)/4
  data <- window(infl, end = origin)
  m <- arima(data, order = c(1,1,0), method = "ML")
  f <- forecast(m, h = hmax)
  AR1_models[[i]] <- m
  AR1_forecasts[[i]] <- f
  AR1_results[i,] <- f$mean
}
rm(i,origin,data,m,f)

AR1_results_list <- lapply(AR1_forecasts, function(x) x$mean)
AR1_results_plot <-  do.call(ts.union, AR1_results_list)
rm(AR1_results_list)

ts.plot(AR1_results_plot,
        main = "Recursive AR(1) forecasts",
        ylab = "Inflation Rate",
        xlab = "",
        lty = "dotted",
        lwd = "2",
        col = "red")
lines(window(infl, start = origin_1),
      lwd = "2",
      col = "blue")
legend("topleft",
       legend = c("Inflation rate", "Forecast"),
       lty = c("solid", "dotted"),
       lwd = c("2", "2"),
       col = c("blue", "red")
)

############################################################################################

# Forecast errors

# First we create four time series starting at the first forecast origin which contains the 
# actual inflation rate for the forecasted horizons.

outcome <- ts(data = matrix(NA, nrow = n_origins, ncol = hmax),
              start = origin_1,
              frequency = frequency(infl))

for (i in 1:n_origins) {
  start_date <- origin_1 + i / 4
  end_date <- start_date + 3/4
  outcome[i,] <- window(infl, start = start_date, end = end_date)
}
rm(i, start_date, end_date)

# We then simply subtract results from outcome to obtain our forecast errors

ARIMA_errors <- outcome - ARIMA_results
AR1_errors <- outcome - AR1_results
colnames(ARIMA_errors) <- paste0("h = ", 1:hmax)
colnames(AR1_errors) <- paste0("h = ", 1:hmax)
rm(outcome) # no need for this anymore as we're only interested in the errors

############################################################################################

# Bias test

# To test whether our forecast of a given horizon h is biased we can simply regress the 
# forecast error on a constant and check whether the estimated coefficient (= mean 
# forecast error) is significant.

bias_test <- function(series,h) {
  model <- dynlm(series[, h] ~ 1)
  test <- coeftest(model, vcov. = NeweyWest(model, lag = h - 1))
  pval <- test[1, 4]
  return(pval)
}

# Note that we used Newey-West standard errors and we need to specify that lag = h - 1.

# Let's create an empty data frame to store our p-values.

bias_pvals <- data.frame(h = 1:hmax, ARIMA = NA, AR1 = NA)

# Then use a loop to fill it with our p-values.

for (h in 1:hmax) {
  bias_pvals$ARIMA[h] <- bias_test(ARIMA_errors,h)
  bias_pvals$AR1[h] <- bias_test(AR1_errors,h)
}
rm(h)

# Results!

bias_pvals

# The null no bias (i.e. expected forecast error = 0) cannot be rejected at any conventional
# significance level except the AR(1) forecast for horizon h = 1.

############################################################################################

# Diebold-Mariano test

# The null hypothesis of the Diebold-Mariano test is that the expected difference in average
# forecast error between two forecasts is equal to zero.

# To perform a Diebold-Mariano test we use the dm.test() function

# First we create an empty dataframe to store the p-values from the Diebold-Mariano test for
# the four different forecast horizons.

DM_pvals <- data.frame(h = 1:hmax,
                         pval = NA)

# Then we fill it with p-values from the test

for (h in 1:hmax) {
  test <- dm.test(ARIMA_errors[,h], AR1_errors[, h], h = h)
  DM_pvals$pval[h] <- test$p.value
}
rm(h,test)

# Results!

DM_pvals

# The null (expected difference in average forecast error = 0) can not be rejected for any
# horizon h = 1,...,4.








# Load libraries

library(urca) # ur.df() and ur.kpss()
library(dynlm) # dynlm()
library(vars) # VARselect, VAR(), serial.test(), normality.test(), causality(), irf(), fanchart(), fevd()
library(forecast) # forecast()

# Load data

data(Macrodat, package = "Ecdat")

CPI <- Macrodat[,"punew"]
inflation_qtq <- diff(log(CPI)) # inflation quarter-to-quarter
inflation <- 100*((1 + inflation_qtq)^4 - 1) # annualized inflation
unemployment <- Macrodat[,"lhur"]

# To combine inflation and unemployment to a multivariate time series we use the
# function ts.intersect().

data <- ts.intersect(inflation,unemployment)

# Plot 

ts.plot(data,
        main = "US Inflation- and Unemployment Rate",
        ylab = "%",
        xlab = "",
        lwd = c(2,2),
        lty = c(1,3)) # 1 = solid, 2 = dashed
legend("topright",
       legend = c("Inflation", "Unemployment"),
       lty = c(1,3)
)

########################################################################################

# We now want to perform an ADF test to determine the order of integration of both
# series. We use the ur.df() function to perform the ADF test. We include the argument 
# selectlags = "AIC" so that the order of the AR(p) model we test for a unit root 
# is selected by AIC (AIC is prefered over BIC, see S&W p.587).

# The t-value on z.lag.1 (or the first number after "Value of test-statistic is:")
# is our test statistic. The p-value becomes smaller as the test statistic becomes 
# more negative, i.e. to reject the null hypothesis of a unit root we need a big
# negative number.

summary(ur.df(data[,"inflation"], type = "none", selectlags = "AIC")) # cannot reject
summary(ur.df(data[,"inflation"], type = "drift", selectlags = "AIC")) # reject at 5%
summary(ur.df(data[,"inflation"], type = "trend", selectlags = "AIC")) # cannot reject

summary(ur.df(data[,"unemployment"], type = "none", selectlags = "AIC")) # cannot reject
summary(ur.df(data[,"unemployment"], type = "drift", selectlags = "AIC")) # reject at 10%
summary(ur.df(data[,"unemployment"], type = "trend", selectlags = "AIC")) # cannot reject

# We can also perform a KPSS test.
# To not reject the null hypothesis of trend stationarity, we need a small number.
# The alternative hypothesis of the KPSS test is existence of a unit root.

summary(ur.kpss(data[,"inflation"])) # reject at 5%
summary(ur.kpss(data[,"unemployment"])) # reject at 5%

# To summarize reject ADF null = GOOD, reject KPSS null = BAD!
# What to do? Difference and then perform tests again! 

diff_data <- diff(data)

# I would have done e.g.:
# data <- ts.intersect(diff(data[,"inflation"]),unemployment),
# if we could reject that unemployment followed a unit root.

summary(ur.df(diff_data[,"inflation"], type = "none", selectlags = "AIC")) # reject at 1%
summary(ur.df(diff_data[,"inflation"], type = "drift", selectlags = "AIC")) # reject at 1%
summary(ur.df(diff_data[,"inflation"], type = "trend", selectlags = "AIC")) # reject at 1%

summary(ur.df(diff_data[,"unemployment"], type = "none", selectlags = "AIC")) # reject at 1%
summary(ur.df(diff_data[,"unemployment"], type = "drift", selectlags = "AIC")) # reject at 1%
summary(ur.df(diff_data[,"unemployment"], type = "trend", selectlags = "AIC")) # reject at 1%

summary(ur.kpss(diff_data[,"inflation"])) # cannot reject
summary(ur.kpss(diff_data[,"unemployment"])) # cannot reject

# Unemployment and inflation are both integrated of order 1

########################################################################################

# We now test for cointegration. The series X and Y are said to be cointegrated if 
# they have the same order of integration d but aX + bY has order of integration 
# < d for some coefficients a and b.

# Inflation and unemployment are both integrated of order 1 (stationary in differences) 
# so they are cointegrated if a linear combination of them is integrated of order 
# 0 (i.e. stationary in levels).

# To test whether inflation and unemployment are cointegrated we perform an 
# Engle-Granger test. How does the Engle-Granger test work? The residuals from
# a regression of one variable on the other has a unit root when they are not 
# cointegrated so perform an "ADF" test with drift on the residuals.

regression <- dynlm(inflation ~ unemployment, data = data)
summary(ur.df(regression$residuals, type = "drift", selectlags = "AIC"))

# The test statistic reported by ur.df() is correct but the critical values are
# not. The test stastic should be compared to the table in S&W p.665 which I've
# reproduced below.

# Significance:  10%      5%      1%
# 1 regressors: -3.12 # -3.41 # -3.96
# 2 regressors: -3.52 # -3.80 # -4.36
# 3 regressors: -3.84 # -4.16 # -4.73
# 4 regressors: -4.20 # -4.49 # -5.07

# The test statistic is -3.0868 and we have 1 regressor so we cannot reject the 
# null of a unit root and thus can reject that inflation and unemployment are 
# cointegrated.

########################################################################################

# We use VARselect() to determine the appropriate lag order:

VARselect(diff_data, type = "const") # allow for a constant

# Most common choice is AIC which suggests p = 4.
# The VAR() function estimates our reduced form VAR.

VAR <- VAR(diff_data, p = 4, type = "const")
summary(VAR)

# Remember that the parameters of the reduced form VAR are essentially impossible 
# to interpret.

########################################################################################

# Let's do some model diagnostics.

# To test whether the residuals are serially correlated we use the serial.test
# function to perform a multivariate Ljung-Box test, i.e. the null hypothesis is
# no serial correlation.

# What is the appropriate lag length to test for? Sufficiently larger than p!
# I would suggest testing 2p, 3p and 4p (here 8, 12,16). Remember that the null
# hypothesis is no serial correlation up to whatever chosen lag length.

serial.test(VAR, lags.pt = 8) # can reject null at 10%
serial.test(VAR, lags.pt = 12) # cannot reject null
serial.test(VAR, lags.pt = 16) # cannot reject null

# We now test if the residuals are normally distributed by performing a multivariate 
# Jarque-Bera test using normality.test()

normality.test(VAR)$jb.mul$JB # $jb.mul$JB to get the results from the J-B test

# The null is that the residuals are normally distributed which we reject. We
# always make note of this but remember that this is not technically a defect
# of the VAR() model.

########################################################################################

# A variable is said to Granger cause another if it predicts it, i.e. X Granger
# causes Y if X can be used to forecast Y. Whether a variable Granger causes 
# another can be tested using causality().

Granger_i_u <- causality(VAR, cause = "inflation", vcov. = sandwich::vcovHC(VAR))
Granger_u_i <- causality(VAR, cause = "unemployment", vcov. = sandwich::vcovHC(VAR))

# causality() performs two tests but we're only interested in the Granger test.
# The null of the first Granger test is that inflation does not Granger cause
# unemployment and the null of the second is that unemployment does not Granger
# cause inflation.

Granger_i_u$Granger # cannot reject null at 10% significance
Granger_u_i$Granger # can reject null, i.e. unemployment Granger causes inflation!

########################################################################################

# The irf() function with ortho = TRUE computes the impulse response functions using
# the Cholesky decomposition, i.e. the order in data matters (here assumed that 
# unemployment has no contemporaneous effect on inflation).

# You can use argument boot = FALSE to get rid of confidence intervals

IRF_iu <- irf(VAR, impulse = "inflation", response = "unemployment", ortho = TRUE, boot = FALSE)
IRF_ui <- irf(VAR, impulse = "unemployment", response = "unemployment", ortho = TRUE, boot = FALSE)
plot(IRF_iu)
plot(IRF_ui)

########################################################################################

# Unemployment Granger causes inflation as established above.

hmax = 40 # 10 years in quarters

# Use forecast() to forecast using our estimated VAR and include fan = TRUE

VAR_forecast <- forecast(VAR, h = hmax, fan = TRUE) 

# Then plot!

plot(VAR_forecast$forecast$inflation,
     main = "US Inflation Rate",
     ylab = "%")

########################################################################################

# We can use the fevd() function to compute the forecast error variance decompositions.

FEVD <- fevd(VAR)

# The round() function simply rounds the output (here to 1 decimal).

round(FEVD$inflation*100,1)
round(FEVD$unemployment*100,1)

# It is also possible to simply type:

FEVD