##########################################################################################################
#                                   VAR & VECM MODEL
##########################################################################################################

# Package installation
# Load or install required packages
required_packages <- c("readr", "dplyr", "xts", "urca", "dynlm", "vars", "forecast")

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
usd_real_exchange_xts <- xts(coffee_data$PTAX, order.by = coffee_data$Date)

# 3. Merge series on common dates
# Merge the four xts objects with inner join on common dates
data_xts <- merge(arabica_spot_xts, robusta_spot_xts, arabica_futures_xts, usd_real_exchange_xts, join = "inner")

# Rename columns
colnames(data_xts) <- c("arabica_spot_price", "robusta_spot_price", "arabica_futures_price", "usd_real_exchange")


diff_log_spot_price_arabica <- na.omit(diff(log(arabica_spot_xts)))
diff_log_spot_price_robusta <- na.omit(diff(log(robusta_spot_xts)))
diff_log_futures_price_arabica <- na.omit(diff(log(arabica_futures_xts)))
diff_log_usd_real_exchange <- na.omit(diff(log(usd_real_exchange_xts)))

##########################################################################################################
###                               1. VAR                                                               ### 

data_vars <- cbind(diff_log_spot_price_arabica,diff_log_spot_price_robusta, diff_log_usd_real_exchange)
VARselect(data_vars, type = "const")

#AIC(n)  HQ(n)  SC(n) FPE(n)
#     3      3      1      3

# Make the VAR model with the selected lag order
model_var <- VAR(data_vars, p = 3, type = "const")  # p from selection

summary(model_var)
#A depreciation of the Brazilian Real against the USD 
#(i.e., increase in USD/BRL exchange rate) likely increases the USD price 
#of Brazilian coffee exports. Since Brazil is a major coffee exporter, 
#this can push up coffee prices globally.

summary(model_var)


serial.test(model_var, lags.pt = 3) #No degrees of freedom — test isn’t valid here
serial.test(model_var, lags.pt = 3, type = "BG") # Therefore I used a  Breusch-Godfrey LM test indicating the
# residuals does not show significant autocorrelation — ableit marginally (0.06075)
serial.test(model_var, lags.pt = 4) #No evidence of serial correlation at lag 4
serial.test(model_var, lags.pt = 7) #Slight evidence, borderline insignificant serial correlation
serial.test(model_var, lags.pt = 10) #Significant serial correlation at lag 10
serial.test(model_var, lags.pt = 15) #Strong evidence of serial correlation at lag 15 (problematic)

normality.test(model_var)$jb.mul$JB 
#The null hypothesis of the Jarque-Bera test is that the residuals are normally distributed, 
#which we reject based on the test result.
#The VAR models can still be useful even if residuals deviate from normality, 
#though inference might be affected.

Granger_arabica <- causality(model_var, cause = "diff_log_spot_price_arabica")
Granger_robusta <- causality(model_var, cause = "diff_log_spot_price_robusta")
Granger_exchange <- causality(model_var, cause = "diff_log_usd_real_exchange")

Granger_arabica$Granger # strong evidence that diff_log_spot_price_arabica Granger-causes the other variables
Granger_robusta$Granger # strong  evidence that changes in diff_log_spot_price_robusta Granger-cause changes in diff_log_spot_price_arabica and diff_log_usd_real_exchange.
Granger_exchange$Granger # Changes in the real exchange rate (diff_log_usd_real_exchange) help predict the future values of both arabica and robusta coffee spot prices.

# IRF: effect of arabica shock on robusta
IRF_arabica_robusta <- irf(model_var, 
                           impulse = "diff_log_spot_price_arabica", 
                           response = "diff_log_spot_price_robusta", 
                           ortho = TRUE, 
                            boot = TRUE, ci = 0.95)


# IRF: effect of robusta shock on arabica
IRF_robusta_arabica <- irf(model_var, 
                           impulse = "diff_log_spot_price_robusta", 
                           response = "diff_log_spot_price_arabica", 
                           ortho = TRUE, 
                           boot = TRUE, ci = 0.95)


# IRF: effect of exhange rate shock on arabica
IRF_exchange_arabica <- irf(model_var, 
                           impulse = "diff_log_usd_real_exchange", 
                           response = "diff_log_spot_price_arabica", 
                           ortho = TRUE, 
                           boot = TRUE, ci = 0.95)


# IRF: effect of exhange rate shock on arabica
IRF_exchange_robusta <- irf(model_var, 
                           impulse = "diff_log_usd_real_exchange", 
                           response = "diff_log_spot_price_robusta", 
                           ortho = TRUE, 
                           boot = TRUE, ci = 0.95)



pdf("Processed_Data/graph_6_IRF_plots.pdf", width = 8, height = 8)

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

plot(IRF_arabica_robusta, main = "Arabica shock on Robusta")
plot(IRF_robusta_arabica, main = "Robusta shock on Arabica")
plot(IRF_exchange_arabica, main = "Exchange rate shock on Arabica")
plot(IRF_exchange_robusta, main = "Exchange rate shock on Robusta")

par(mfrow = c(1, 1))

dev.off()



##########################################################################################################
###                               2. VECM                                                              ### 

# Combine into a matrix or dataframe
data_vecm <- cbind(arabica_spot_xts, arabica_futures_xts)

# Johansen cointegration test
jo_test <- ca.jo(data_vecm, type = "trace", ecdet = "const", K = 2)  
# K is number of lags in the VAR in levels (adjust based on AIC/BIC)
summary(jo_test)

# If cointegration rank (r) >= 1, proceed to fit VECM
vecm_model <- cajorls(jo_test, r = 1)  # r=1 means one cointegrating vector

summary(vecm_model$rlm)  # see the VECM regression results

