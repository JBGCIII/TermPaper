##########################################################################################################
###                               STATIONARY & COINTEGRATION TEST                                      ### 
##########################################################################################################
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
usd_real_exchange_xts <- xts(coffee_data$PTAX, order.by = coffee_data$Date)

# Pairwise merge using join = "inner"
temp1 <- merge(arabica_spot_xts, robusta_spot_xts, join = "inner")
temp2 <- merge(temp1, arabica_futures_xts, join = "inner")
data_xts <- merge(temp2, usd_real_exchange_xts, join = "inner")

# Rename columns
colnames(data_xts) <- c("arabica_spot_price", "robusta_spot_price", "arabica_futures_price", "usd_real_exchange")

##########################################################################################################
###                               1. Unit Root Test                                                    ### 

# 1. Unit root tests – Spot Price Arabica
summary(ur.df(arabica_spot_xts, type = "none", selectlags = "AIC")) 
summary(ur.df(arabica_spot_xts, type = "drift", selectlags = "AIC")) 
summary(ur.df(arabica_spot_xts, type = "trend", selectlags = "AIC"))

# 2. Unit root tests – Spot Price Robusta
summary(ur.df(robusta_spot_xts, type = "none", selectlags = "AIC"))
summary(ur.df(robusta_spot_xts, type = "drift", selectlags = "AIC"))
summary(ur.df(robusta_spot_xts, type = "trend", selectlags = "AIC"))

# 3. Unit root tests – Futures Price
summary(ur.df(arabica_futures_xts, type = "none", selectlags = "AIC"))
summary(ur.df(arabica_futures_xts, type = "drift", selectlags = "AIC"))
summary(ur.df(arabica_futures_xts, type = "trend", selectlags = "AIC"))

# 4. Unit root tests – Exchange Rate
summary(ur.df(usd_real_exchange_xts, type = "none", selectlags = "AIC"))
summary(ur.df(usd_real_exchange_xts, type = "drift", selectlags = "AIC"))
summary(ur.df(usd_real_exchange_xts, type = "trend", selectlags = "AIC"))

##########################################################################################################
###                               2. Unit Root Test (Log)                                                   ### 

# 1. Log transformation
log_spot_price_arabica <- log(arabica_spot_xts)
log_spot_price_robusta <- log(robusta_spot_xts)
log_futures_price_arabica <- log(arabica_futures_xts)
log_usd_real_exchange_xts<- log(usd_real_exchange_xts)

# 2. Unit root tests – Spot Price Arabica
summary(ur.df(log_spot_price_arabica, type = "none", selectlags = "AIC"))
summary(ur.df(log_spot_price_arabica, type = "drift", selectlags = "AIC"))
summary(ur.df(log_spot_price_arabica, type = "trend", selectlags = "AIC"))

# 3. Unit root tests – Spot Price Robusta
summary(ur.df(log_spot_price_robusta, type = "none", selectlags = "AIC"))
summary(ur.df(log_spot_price_robusta, type = "drift", selectlags = "AIC"))
summary(ur.df(log_spot_price_robusta, type = "trend", selectlags = "AIC"))

# 4. Unit root tests – Futures Price
summary(ur.df(log_futures_price_arabica, type = "none", selectlags = "AIC"))
summary(ur.df(log_futures_price_arabica, type = "drift", selectlags = "AIC"))
summary(ur.df(log_futures_price_arabica, type = "trend", selectlags = "AIC"))

# 5. Unit root tests – Exchange Rate
summary(ur.df(log_usd_real_exchange_xts, type = "none", selectlags = "AIC"))
summary(ur.df(log_usd_real_exchange_xts, type = "drift", selectlags = "AIC"))
summary(ur.df(log_usd_real_exchange_xts, type = "trend", selectlags = "AIC"))

##########################################################################################################
###                               3. Unit Root Test (Difference of Logs)                                                    ### 

# 1. Difference of Logs
diff_log_spot_price_arabica <- na.omit(diff(log_spot_price_arabica))
diff_log_spot_price_robusta <- na.omit(diff(log_spot_price_robusta))
diff_log_futures_price_arabica <- na.omit(diff(log_futures_price_arabica))
diff_log_exchange_rates <- na.omit(diff(usd_real_exchange_xts))


# 2. Unit root tests – Log Returns (Diff Log)
summary(ur.df(diff_log_spot_price_arabica, type = "none", selectlags = "AIC"))
summary(ur.df(diff_log_spot_price_arabica, type = "drift", selectlags = "AIC"))
summary(ur.df(diff_log_spot_price_arabica, type = "trend", selectlags = "AIC"))

# 3. Unit root tests – Spot Price Robusta
summary(ur.df(diff_log_spot_price_robusta, type = "none", selectlags = "AIC"))
summary(ur.df(diff_log_spot_price_robusta, type = "drift", selectlags = "AIC"))
summary(ur.df(diff_log_spot_price_robusta, type = "trend", selectlags = "AIC"))

# 4. Unit root tests – Futures Price
summary(ur.df(diff_log_futures_price_arabica, type = "none", selectlags = "AIC"))
summary(ur.df(diff_log_futures_price_arabica, type = "drift", selectlags = "AIC"))
summary(ur.df(diff_log_futures_price_arabica, type = "trend", selectlags = "AIC"))

# 5. Unit root tests – Exchange Rate
summary(ur.df(diff_log_exchange_rates, type = "none", selectlags = "AIC"))
summary(ur.df(diff_log_exchange_rates, type = "drift", selectlags = "AIC"))
summary(ur.df(diff_log_exchange_rates, type = "trend", selectlags = "AIC"))


#log prices are I(1) processes (integrated of order 1).

##########################################################################################################
###                                           KPSS TEST                                                ### 
##########################################################################################################

# 1.KPPS
summary(ur.kpss(arabica_spot_xts))
summary(ur.kpss(robusta_spot_xts))
summary(ur.kpss(arabica_futures_xts))
summary(ur.kpss(usd_real_exchange_xts))


# 2.KPPS Log
summary(ur.kpss(log_spot_price_arabica))
summary(ur.kpss(log_spot_price_robusta))
summary(ur.kpss(log_futures_price_arabica))
summary(ur.kpss(log_usd_real_exchange_xts))


# 3.KPPS Difference Log
summary(ur.kpss(diff_log_spot_price_arabica))
summary(ur.kpss(diff_log_spot_price_robusta))
summary(ur.kpss(diff_log_futures_price_arabica))
summary(ur.kpss(diff_log_exchange_rates))


##########################################################################################################
###                                           COINTEGRATION TEST                                       ### 
##########################################################################################################

# Arabica spot price and Arabica futures price
regression <- dynlm(Price_Arabica ~ Close_USD_60kg, data = coffee_data)
summary(ur.df(regression$residuals, type = "drift", selectlags = "AIC"))
# The residuals are stationary ⇒ Price_Arabica and Close_USD_60kg share a long-run equilibrium relationship (cointegrated)

# Arabica spot price and Robusta spot price
regression <- dynlm(Price_Arabica ~ Price_Robusta, data = coffee_data)
summary(ur.df(regression$residuals, type = "drift", selectlags = "AIC"))
# The residuals are not stationary ⇒ No evidence of a long-run equilibrium relationship (no cointegration) between Price_Arabica and Price_Robusta

# Arabica spot price and exchange rate (PTAX)
regression <- dynlm(Price_Arabica ~ PTAX, data = coffee_data)
summary(ur.df(regression$residuals, type = "drift", selectlags = "AIC"))
# The residuals are not stationary ⇒ No evidence of cointegration between Price_Arabica and PTAX

# Robusta spot price and exchange rate (PTAX)
regression <- dynlm(Price_Robusta ~ PTAX, data = coffee_data)
summary(ur.df(regression$residuals, type = "drift", selectlags = "AIC"))
# The residuals are not stationary ⇒ No evidence of cointegration between Price_Robusta and PTAX