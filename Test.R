##########################################################################################################
#                                         DATA PROCESSING
##########################################################################################################
library(urca)
library(dplyr)
library(tidyr)

# --- Example data creation (only run if these objects do not already exist) ---
spot_vec <- ts(rnorm(100))
futures_vec <- ts(rnorm(100))
log_spot <- log(spot_vec)
log_futures <- log(futures_vec)
diff_log_spot <- diff(log_spot)
diff_log_futures <- diff(log_futures)

# --- Function to run ADF test and extract key info ---
extract_adf_results <- function(ts_data, series_name) {
  test_types <- c("none", "drift", "trend")

  results_list <- lapply(test_types, function(test_type) {
    adf_test <- ur.df(ts_data, type = test_type, selectlags = "AIC")
    
    # Extract test statistic (tau) — first test stat
    test_stat <- adf_test@teststat[1]
    
    # Extract critical values (named vector)
    crit_vals <- adf_test@cval[1,]
    
    tibble(
      Series = series_name,
      Test_Type = test_type,
      Test_Statistic = test_stat,
      Critical_1pct = crit_vals["1pct"],
      Critical_5pct = crit_vals["5pct"],
      Critical_10pct = crit_vals["10pct"],
    )
  })

  bind_rows(results_list)
}

# --- Prepare series list with only existing objects ---
series_names <- c("spot_vec", "futures_vec", "log_spot", "log_futures", "diff_log_spot", "diff_log_futures")
existing_series <- series_names[sapply(series_names, exists)]

# Create named list of the existing series objects
series_list <- lapply(existing_series, get)
names(series_list) <- existing_series

# --- Run tests on all series and combine results ---
all_results <- bind_rows(
  lapply(names(series_list), function(name) {
    extract_adf_results(series_list[[name]], series_name = name)
  })
)

# --- Save results to CSV ---
write.csv(all_results, "Processed_Data/ADF_Test_Results.csv", row.names = FALSE)







library(urca)
library(dplyr)

extract_kpss_results <- function(ts_data, series_name) {
  kpss_test <- ur.kpss(ts_data)
  sum_kpss <- summary(kpss_test)
  
  test_stat <- kpss_test@teststat
  crit_vals <- kpss_test@cval
  
  tibble(
    Series = series_name,
    Test_Statistic = test_stat,
    Critical_10pct = crit_vals["10pct"],
    Critical_5pct = crit_vals["5pct"],
    Critical_2_5pct = crit_vals["2.5pct"],
    Critical_1pct = crit_vals["1pct"]
  )
}

# Define your series (replace these with your actual vectors)
series_list <- list(
  spot_vec = spot_vec,
  futures_vec = futures_vec,
  log_spot = log_spot,
  log_futures = log_futures,
  diff_log_spot = diff_log_spot,
  diff_log_futures = diff_log_futures
)

# Run KPSS test on all and bind results
all_kpss_results <- bind_rows(
  lapply(names(series_list), function(name) {
    extract_kpss_results(series_list[[name]], name)
  })
)

print(all_kpss_results)









library(urca)
library(dplyr)

# Function to extract ADF test results
extract_adf_results <- function(ts_data, series_name) {
  test_types <- c("none", "drift", "trend")
  
  results_list <- lapply(test_types, function(test_type) {
    adf_test <- ur.df(ts_data, type = test_type, selectlags = "AIC")
    test_stat <- adf_test@teststat[1]
    crit_vals <- adf_test@cval[1, ]
    
    tibble(
      Series = series_name,
      Test = "ADF",
      Test_Type = test_type,
      Test_Statistic = test_stat,
      Critical_1pct = crit_vals["1pct"],
      Critical_5pct = crit_vals["5pct"],
      Critical_10pct = crit_vals["10pct"],
      P_value = NA  # Not provided by ur.df
    )
  })
  
  bind_rows(results_list)
}

# Function to extract KPSS test results
extract_kpss_results <- function(ts_data, series_name) {
  kpss_test <- ur.kpss(ts_data)
  test_stat <- kpss_test@teststat
  crit_vals <- kpss_test@cval
  
  tibble(
    Series = series_name,
    Test = "KPSS",
    Test_Type = "level",  # Adjust if you want trend test
    Test_Statistic = test_stat,
    Critical_1pct = crit_vals["1pct"],
    Critical_5pct = crit_vals["5pct"],
    Critical_10pct = crit_vals["10pct"],
    P_value = NA
  )
}

# Your series list (make sure these are loaded in your environment)
series_list <- list(
  spot_vec = spot_vec,
  futures_vec = futures_vec,
  log_spot = log_spot,
  log_futures = log_futures,
  diff_log_spot = diff_log_spot,
  diff_log_futures = diff_log_futures
)

# Run all ADF tests and combine results
all_adf_results <- bind_rows(
  lapply(names(series_list), function(name) {
    extract_adf_results(series_list[[name]], name)
  })
)

# Run all KPSS tests and combine results
all_kpss_results <- bind_rows(
  lapply(names(series_list), function(name) {
    extract_kpss_results(series_list[[name]], name)
  })
)

# Combine ADF and KPSS results into one table
all_results <- bind_rows(all_adf_results, all_kpss_results)

# Save results to CSV
write.csv(all_results, "UnitRootTestResults.csv", row.names = FALSE)

# Optional: print the combined results
print(all_results)



























# Load libraries
library(readr)
library(dplyr)
library(xts)
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
summary(ur.df(spot_vec, type = "none", selectlags = "AIC"))    # ADF on spot price, no trend
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

# Cointegration test: regress spot on futures and test residuals for unit root
df_for_dynlm <- data.frame(
  arabica_spot_price = spot_vec,
  arabica_futures_price = futures_vec
)

regression <- dynlm(arabica_spot_price ~ arabica_futures_price, data = df_for_dynlm)

summary(ur.df(residuals(regression), type = "drift", selectlags = "AIC"))











# We now want to perform an ADF test to determine the order of integration of both
# series. We use the ur.df() function to perform the ADF test. We include the argument 
# selectlags = "AIC" so that the order of the AR(p) model we test for a unit root 
# is selected by AIC (AIC is prefered over BIC, see S&W p.587).

# The t-value on z.lag.1 (or the first number after "Value of test-statistic is:")
# is our test statistic. The p-value becomes smaller as the test statistic becomes 
# more negative, i.e. to reject the null hypothesis of a unit root we need a big
# negative number.

summary(ur.df(data[,"arabica_spot_price"], type = "none", selectlags = "AIC")) # Cannot Reject H₀. 
summary(ur.df(data[,"arabica_spot_price"], type = "drift", selectlags = "AIC")) # Cannot Reject H₀. 
summary(ur.df(data[,"arabica_spot_price"], type = "trend", selectlags = "AIC")) # Cannot Reject H₀. 

summary(ur.df(data[,"arabica_futures_price"], type = "none", selectlags = "AIC")) # Cannot Reject H₀. 
summary(ur.df(data[,"arabica_futures_price"], type = "drift", selectlags = "AIC")) # Cannot Reject H₀. 
summary(ur.df(data[,"arabica_futures_price"], type = "trend", selectlags = "AIC")) # Cannot Reject H₀. 

summary(ur.kpss(data[,"arabica_spot_price"])) # we reject the null hypothesis.
summary(ur.kpss(data[,"arabica_futures_price"])) # we reject the null hypothesis.


#############################################################################################################

# Example in R
log_spot <- log(data[,"arabica_spot_price"])
log_futures <- log(data[,"arabica_futures_price"])


summary(ur.df(log_spot, type = "none", selectlags = "AIC")) # Cannot Reject H₀. 
summary(ur.df(log_spot, type = "drift", selectlags = "AIC")) # Cannot Reject H₀. 
summary(ur.df(log_spot , type = "trend", selectlags = "AIC")) # Cannot Reject H₀. 

summary(ur.df(log_futures, type = "none", selectlags = "AIC")) # Cannot Reject H₀. 
summary(ur.df(log_futures, type = "drift", selectlags = "AIC")) # Cannot Reject H₀. 
summary(ur.df(log_futures, type = "trend", selectlags = "AIC")) # Cannot Reject H₀. 

summary(ur.kpss(log_spot)) #we reject H₀ → Not Stationary
summary(ur.kpss(log_futures)) #we reject H₀ →  Not Stationary

####################################################################################################################

diff_log_spot <- diff(log_spot)
diff_log_futures <- diff(log_futures)

summary(ur.df(diff_log_spot, type = "none", selectlags = "AIC")) # Reject H₀ → Stationary
summary(ur.df(diff_log_spot, type = "drift", selectlags = "AIC")) # Reject H₀ → Stationary
summary(ur.df(diff_log_spot, type = "trend", selectlags = "AIC")) # Reject H₀ → Stationary

summary(ur.df(diff_log_futures, type = "none", selectlags = "AIC")) # Reject H₀ → Stationary
summary(ur.df(diff_log_futures, type = "drift", selectlags = "AIC")) # Reject H₀ → Stationary
summary(ur.df(diff_log_futures, type = "trend", selectlags = "AIC")) # Reject H₀ → Stationary


summary(ur.kpss(diff_log_spot)) #Fail to reject H₀ → Stationary
summary(ur.kpss(diff_log_futures)) #Fail to reject H₀ → Stationary


########################################################################################

regression <- dynlm(arabica_spot_price ~ arabica_futures_price, data = data)
summary(ur.df(regression$residuals, type = "drift", selectlags = "AIC"))




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

regression <- dynlm(arabica_spot_price ~ arabica_futures_price, data = data)
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

#

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





















data <- ts.intersect(inflation,unemployment)


data(Macrodat, package = "Ecdat")

CPI <- Macrodat[,"punew"]
inflation_qtq <- diff(log(CPI)) # inflation quarter-to-quarter
inflation <- 100*((1 + inflation_qtq)^4 - 1) # annualized inflation
unemployment <- Macrodat[,"lhur"]

# To combine inflation and unemployment to a multivariate time series we use the
# function ts.intersect().

data <- ts.intersect(inflation,unemployment)


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

#

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