
#==================================================================================================
# Here are the necessary packages for all the porblems
library(Ecdat)
library(dynlm)
library(tseries)
library(lmtest)
library(forecast)
library(pxweb)
library(zoo)
library(sandwich)
library(dplyr)
library(lubridate)

#==================================================================================================

################################################################################################### 
# Problem I
#
# Use the variable income, quarterly disposable household income in nominal pounds 1971:Q1-1985:Q2,
# from the dataset IncomeUK in the Ecdat package.
#
################################################################################################### 
# 
# a) Use the first order difference of income to estimate the five models,
#    AR(p) where p= 1,2,3,4,5. The models should be estimated with lm() or dynlm(),  
#    the latter from the package with the same name. Make sure that the same number of 
#    observations are used to estimatethe different models.
#==================================================================================================

# Load the dataset, as a data frame to be able to manipulate it eaiser
IncomeUK <- as.data.frame(Ecdat::IncomeUK)

# Compute first difference of income
diff_income <- diff(IncomeUK$income)

# Use embed to prepare lag matrix for AR(1) to AR(5)
# Embed with dimension = 6 to allow for up to 5 lags
diff_income_embed <- embed(diff_income, dimension = 6)

# Convert to time series
diff_income_ts <- ts(diff_income_embed, start = c(1971, 2), frequency = 4)

# Remove unsessesary datasets
rm(diff_income, diff_income_embed, IncomeUK)

# Estimate AR(1) to AR(5) using dynlm in a list for easy access
models <- list()
models[[1]] <- dynlm(diff_income_ts[,1] ~ diff_income_ts[,2:2])
models[[2]] <- dynlm(diff_income_ts[,1] ~ diff_income_ts[,2:3])
models[[3]] <- dynlm(diff_income_ts[,1] ~ diff_income_ts[,2:4])
models[[4]] <- dynlm(diff_income_ts[,1] ~ diff_income_ts[,2:5])
models[[5]] <- dynlm(diff_income_ts[,1] ~ diff_income_ts[,2:6])

# Print summaries
for (p in 1:5) {
  cat("\nSummary of AR(", p, ") model:\n", sep = "")
  print(summary(models[[p]]))
}

#==================================================================================================
#
# b) Use Akaike’s information criterion (AIC) and 
#    compare the models. Which model should we choose according to AIC?
#
#==================================================================================================

# Calculate AIC for each model
aic_values <- sapply(models, AIC)
aic_values
# Find the model with the minimum AIC
best_model_index <- which.min(aic_values)
cat("\nThe model with the lowest AIC is AR(", best_model_index, ")\n", sep = "")

# The AIC is a measure of the relative quality of a model.
# It is based on the likelihood function and includes a penalty term for the number of parameters
# in the model. The AIC is calculated as:
# AIC = 2 * k - 2*  log(L)
# where L is the likelihood of the model and k is the number of parameters.
# The model with the lowest AIC is considered the best among the set of models being compared.
# The AIC is useful for model selection, especially when comparing models with different numbers
# of parameters. It helps to avoid overfitting by penalizing models with more parameters.
# However, it is important to note that the AIC is not an absolute measure of model quality.
# It is only useful for comparing models within the same dataset. A lower AIC value indicates a
# better fit to the data, but it does not provide information about the goodness of fit in an absolute sense.
# In this case, the AIC values for the AR(1) to AR(5) models are as follows:
# AR(1):  840.5763
# AR(2):  836.7458
# AR(3):  838.2828
# AR(4):  826.6127
# AR(5):  828.0437
# The AR(4) model has the lowest AIC value of 826.6127, indicating that it is the best model
# among the five AR models considered. This suggests that the AR(4) model provides the best
# balance between model fit and complexity, making it the most suitable choice for this dataset.

#==================================================================================================
#
# c) Perform Ljung-Box and Jarque-Bera tests on the residuals of the mod-els.
#    What are the results? Compare with the results from using AIC.Interpretthe results.
#
#==================================================================================================

# Number of observations used in each model, for the Ljung-Box test
n_obs <- nobs(models[[1]])

# Initial lag using rule-of-thumb
base_q <- floor(0.75 * n_obs^(1/3)) # gives 2, this is very low

# Using 2 lags for the Ljung-Box test will give us NA for AR(2) - AR(5)
# we need to set q to be at least the number of parameters + 1

# Make a data.frame to store results
results <- data.frame(
  Model = character(),
  AIC = numeric(),
  Ljung_Box_p = numeric(),
  Jarque_Bera_p = numeric(),
  stringsAsFactors = FALSE
)

# Loop over models
for (p in 1:5) {
  model <- models[[p]]
  res <- residuals(model)
  n_coeff <- length(coef(model))  # number of parameters, for each model
  
  # Adjust lag to ensure lag > fitdf, otherwise we get NA
  q <- max(base_q, n_coeff + 1) # Use the maximum between base_q or n_coeff + 1, which ever is larger

  # Ljung-Box test
  lb_test <- Box.test(res, lag = q, type = "Ljung-Box", fitdf = n_coeff)

  # Jarque-Bera test
  jb_test <- jarque.bera.test(res)

  # Append results
  results <- rbind(results, data.frame(
    Model = paste0("AR(", p, ")"),
    AIC = AIC(model),
    Ljung_Box_p = lb_test$p.value,
    Jarque_Bera_p = jb_test$p.value
  ))
}
# Shows the AIC, Ljung-Box p-value, and Jarque-Bera p-value for each model.
print(results)
# For the ljung-box-test: null hypothesis of no autocorrelation, bad with low p-value,
# For the jarque-bera-test: null hypothesis of normality, bad with low p-value
# From both tests we can see that AR(4) seems to be the best model, as it has the highest p-values
# for both tests, thus we can not reject the null hypothesis of no autocorrelation and normality.
# Telling us that the residuals are white noise and normally distributed for the AR(4) model.
# We can also assume that the AR(5) also is a decent model, but the AIC is a bitt higher than AR(4)
# and the p-values are lower for the Ljung-Box, we can rejcet the null hypothesis at 90% confidence level.
# The preferred model is thus the AR(4). 
