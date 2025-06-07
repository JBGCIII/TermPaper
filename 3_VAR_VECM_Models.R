##########################################################################################################
#                                   VAR & VECM MODEL
##########################################################################################################



# Load libraries

library(urca) # ur.df() and ur.kpss()
library(dynlm) # dynlm()
library(vars) # VARselect, VAR(), serial.test(), normality.test(), causality(), irf(), fanchart(), fevd()
library(forecast) # forecast()



# Package installation
# Load or install required packages
required_packages <- c("readr", "dplyr", "xts", "urca", "dynlm")

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


# 3. Merge series on common dates
data_xts <- merge(merge(arabica_spot_xts, robusta_spot_xts, join = "inner"), arabica_futures_xts, join = "inner")
colnames(data_xts) <- c("arabica_spot_price", "robusta_spot_price", "arabica_futures_price")


diff_log_spot_price_arabica <- na.omit(diff(log(arabica_spot_xts)))
diff_log_spot_price_robusta <- na.omit(diff(log(robusta_spot_xts)))
diff_log_futures_price_arabica <- na.omit(diff(log(arabica_futures_xts)))


data_vars <- cbind(diff_log_spot_price_arabica,diff_log_spot_price_robusta)
VARselect(data_vars, type = "const")
#                  1             2             3             4             5
#AIC(n) -1.677313e+01 -1.677636e+01 -1.677988e+01 -1.677879e+01 [-1.678003e+01]
#HQ(n)  -1.677067e+01 -1.677226e+01 [-1.677414e+01] -1.677141e+01 -1.677102e+01
#SC(n)  [-1.676607e+01] -1.676460e+01 -1.676341e+01 -1.675762e+01 -1.675415e+01
#FPE(n)  5.194261e-08  5.177487e-08  5.159318e-08  5.164933e-08  [5.158522e-08]
#                   6             7             8             9            10
#AIC(n) -1.677934e+01 -1.677851e+01 -1.677808e+01 -1.677739e+01 -1.677712e+01
#HQ(n)  -1.676869e+01 -1.676622e+01 -1.676415e+01 -1.676182e+01 -1.675992e+01
#SC(n)  -1.674876e+01 -1.674323e+01 -1.673809e+01 -1.673269e+01 -1.672772e+01
#FPE(n)  5.162092e-08  5.166354e-08  5.168597e-08  5.172184e-08  5.173541e-08

#AIC(n) 5, #FPE(n) 5 --> p

# Make the VAR model with the selected lag order
model_var <- VAR(data_vars, p = 5, type = "const")  # p from selection

summary(model_var)









VARselec()

# Fit a VAR model, the variables are not cointegrated, so we need to use a VAR model in differences.
# Combind the two series into a data frame
data_vars <- cbind(diff(egg), diff(chickens))
# Use VARselect to find the optimal lag order based on information criteria
VARselect(data_vars, type = "const")
# It suggests a lag order of 1 based on AIC, HQ, and FPE criteria. We will use this lag order for our VAR model.

# Make the VAR model with the selected lag order
model_var <- VAR(data_vars, p = 1, type = "const")  # p from selection
summary(model_var)
# We cant really intrerpret the coefficients, as we are working with first differences.

#==================================================================================================
#
# 4) Perform Ljung-Box and Jarque-Bera tests. What are the conclusions?
#
#==================================================================================================

# Perform Ljung-Box test multivariate residuals, we chose lags 2, 3, and 4. Using the 2p, 3p and 4p
serial.test(model_var, lags.pt = 2) # cannot reject null
serial.test(model_var, lags.pt = 3) # cannot reject null
serial.test(model_var, lags.pt = 4) # cannot reject null
# The null hypothesis is no serial correlation, and we cannot reject it at any of the tested lags,
# indicating that the residuals are not serially correlated.

# Perform Jarque-Bera test for normality of residuals multivariate
normality.test(model_var)$jb.mul$JB # p-value = 0.3855
# The null is that the residuals are normally distributed, we can not reject the null,
# thus the residuals appear to be normally distributed.

#===================================================================================================
#
# 5) Perform Granger’s causality test. Do chicken Granger-cause eggs or eggs Granger-cause chicken?
#
#===================================================================================================
# Granger causality test for chicken causing egg
Granger_c_e <- causality(model_var, cause = "diff.chickens.", vcov. = sandwich::vcovHC(model_var))
# Grnager causality test for egg causing chicken
Granger_e_c <- causality(model_var, cause = "diff.egg.", vcov. = sandwich::vcovHC(model_var))


# Check the results of the Granger causality tests
# The null of the first Granger test is that chickens does not Granger cause
# eggs and the null of the second is that eggs does not Granger
# cause chickens
Granger_c_e$Granger # cannot reject null at 1% significance, p-value = 0.03755
Granger_e_c$Granger # cannot reject null at 10% significance, p-value = 0.4948
# The results indicate that chickens ganger cause eggs at the 5% significance level

#====================================================================================================
#
# 6) So which was first, the chicken or the egg?
#
#====================================================================================================

# The dad joke answer: The chicken came first… but only because the egg hit the snooze button.

# The serious answer: The chicken Granger causes the egg, but not the other way around.
# Thus, we can conclude that the chicken came first in the sense that it can predict the egg production.
# And we assume something evolved in to the chicken, which then laid the eggs. 










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




#==================================================================================================
# Load the necessary packages for all the porblems
library(lmtest) # lmtest::ChickEgg
library(urca) # ur.df() and ur.kpss()
library(dynlm) # dynlm()
library(vars) # VARselect, VAR(), serial.test(), normality.test(), causality(), irf(), fanchart(), fevd()

#==================================================================================================

################################################################################################### 
# Problem I
#
# Use the dataset ChickEgg from lmtest
#
################################################################################################### 

# Load the dataset
data("ChickEgg", package = "lmtest")

egg <- ChickEgg[,"chicken"]
chickens <- ChickEgg[,"egg"]

#==================================================================================================
#
# 1) Which order of integration do the variables have?
#
#==================================================================================================
# Check the order of integration using the Augmented Dickey-Fuller test

# Look at the time series, looks like they are non-stationary
ts.plot(egg, main = "Egg Production Time Series")
ts.plot(chickens, main = "Chicken Population Time Series")
# Look at the first differences, seams to be stationary, we get a clue that they may be I(1)
ts.plot(diff(egg), main = "First Difference of Egg Production")
ts.plot(diff(chickens), main = "First Difference of Chicken Population")

# We do a ADF test on the original series and the first differences
# Check the order of integration for egg
summary((ur.df(egg, type = "none", selectlags = "AIC"))) # Cannot reject null at anny significance level
summary((ur.df(egg, type = "drift", selectlags = "AIC"))) # Cannot reject null at any significance level
summary((ur.df(egg, type = "trend", selectlags = "AIC"))) # Cannotreject null at any significance level

# Check the order of integration for chicken
summary(ur.df(chickens, type = "drift", selectlags = "AIC")) # Cannot reject null at anny significance level
summary(ur.df(chickens, type = "drift", selectlags = "AIC")) # Cannot reject null at anny significance level
summary(ur.df(chickens, type = "trend", selectlags = "AIC")) # Cannot reject null at anny significance level
# Both are non-stationary at level, we need to test the first differences

# Check the first differences for egg
summary(ur.df(diff(egg), type = "none", selectlags = "AIC")) # Can reject null at any significance level
summary(ur.df(diff(egg), type = "drift", selectlags = "AIC")) # Can reject null at any significance level
summary(ur.df(diff(egg), type = "trend", selectlags = "AIC")) # Can reject null at any significance level
# Check the first differences for chicken
summary(ur.df(diff(chickens), type = "none", selectlags = "AIC")) # Can reject null at any significance level
summary(ur.df(diff(chickens), type = "drift", selectlags = "AIC")) # Can reject null at any significance level
summary(ur.df(diff(chickens), type = "trend", selectlags = "AIC")) # Can reject null at any significance level


# We can also use the ur.kpss() function to check for stationarity
summary(ur.kpss(egg)) # We reject at 5 %
summary(ur.kpss(chickens)) # We reject at all significance levels
# Sems like both series are non-stationary at level, as we saw from the ADF test.

# As a final check, we can also look at the KPSS test too be sure we have a I(1)
summary(ur.kpss(diff(egg))) # We cannot reject at any significance level
summary(ur.kpss(diff(chickens))) # We cannot reject at any significance level
# Both first differences are stationary, so we conclude that both variables are I(1).


# Both egg and chiken are stationary after the first diff, so we caan conclude that both variables are I(1).


#==================================================================================================
#
# 2) Show that the variables are not cointegrated
#
#==================================================================================================

# Engle-Granger two-step test:
# Regress egg on chickens
model <- dynlm(egg ~ chickens)
# Take out the residuals
residuals <- resid(model)
# ADF test on residuals (no trend but intercept)
summary(ur.df(residuals, type = "drift", selectlags = "AIC")) # Test statistics -1.8098
# The critical values for the ADF test are:
# Significance:  10%      5%      1%
# 1 regressors: -3.12 # -3.41 # -3.96
# 2 regressors: -3.52 # -3.80 # -4.36
# 3 regressors: -3.84 # -4.16 # -4.73
# 4 regressors: -4.20 # -4.49 # -5.07
# We can see that the residuals are non-stationary, which indicates that the two series are not cointegrated.

#==================================================================================================
#
# 3) Use information criteria to specify a VAR-model of an appropriate lag order.
#
#==================================================================================================
# Fit a VAR model, the variables are not cointegrated, so we need to use a VAR model in differences.
# Combind the two series into a data frame
data_vars <- cbind(diff(egg), diff(chickens))
# Use VARselect to find the optimal lag order based on information criteria
VARselect(data_vars, type = "const")
# It suggests a lag order of 1 based on AIC, HQ, and FPE criteria. We will use this lag order for our VAR model.

# Make the VAR model with the selected lag order
model_var <- VAR(data_vars, p = 1, type = "const")  # p from selection
summary(model_var)
# We cant really intrerpret the coefficients, as we are working with first differences.

#==================================================================================================
#
# 4) Perform Ljung-Box and Jarque-Bera tests. What are the conclusions?
#
#==================================================================================================

# Perform Ljung-Box test multivariate residuals, we chose lags 2, 3, and 4. Using the 2p, 3p and 4p
serial.test(model_var, lags.pt = 2) # cannot reject null
serial.test(model_var, lags.pt = 3) # cannot reject null
serial.test(model_var, lags.pt = 4) # cannot reject null
# The null hypothesis is no serial correlation, and we cannot reject it at any of the tested lags,
# indicating that the residuals are not serially correlated.

# Perform Jarque-Bera test for normality of residuals multivariate
normality.test(model_var)$jb.mul$JB # p-value = 0.3855
# The null is that the residuals are normally distributed, we can not reject the null,
# thus the residuals appear to be normally distributed.

#===================================================================================================
#
# 5) Perform Granger’s causality test. Do chicken Granger-cause eggs or eggs Granger-cause chicken?
#
#===================================================================================================
# Granger causality test for chicken causing egg
Granger_c_e <- causality(model_var, cause = "diff.chickens.", vcov. = sandwich::vcovHC(model_var))
# Grnager causality test for egg causing chicken
Granger_e_c <- causality(model_var, cause = "diff.egg.", vcov. = sandwich::vcovHC(model_var))


# Check the results of the Granger causality tests
# The null of the first Granger test is that chickens does not Granger cause
# eggs and the null of the second is that eggs does not Granger
# cause chickens
Granger_c_e$Granger # cannot reject null at 1% significance, p-value = 0.03755
Granger_e_c$Granger # cannot reject null at 10% significance, p-value = 0.4948
# The results indicate that chickens ganger cause eggs at the 5% significance level

#====================================================================================================
#
# 6) So which was first, the chicken or the egg?
#
#====================================================================================================

# The dad joke answer: The chicken came first… but only because the egg hit the snooze button.

# The serious answer: The chicken Granger causes the egg, but not the other way around.
# Thus, we can conclude that the chicken came first in the sense that it can predict the egg production.
# And we assume something evolved in to the chicken, which then laid the eggs. 