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