


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