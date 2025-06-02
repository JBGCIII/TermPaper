# This is the files that contained extra information that was not included in the main analysis


####################################################################################################################

#This code combines the different CSV files from the central bank before I found the API
#Took Me just 4 extra hours FFJAJFaipwtaötojaötölataöltötamöltamöltwaölmwt!låp12p2å5p

# Load libraries
library(dplyr)
library(readr)
library(lubridate)
library(rbcb)

# 1. Read and clean local CSV files
files <- list.files(path = "Raw_Data/Exchange_Rate/", pattern = "*.csv", full.names = TRUE)

read_clean_file <- function(file) {
  df <- read_delim(file, delim = ";", col_names = FALSE,
                   locale = locale(decimal_mark = ","), 
                   trim_ws = TRUE,
                   show_col_types = FALSE)

  colnames(df) <- c("DateRaw", "Code", "Type", "Currency", "BuyRate", "SellRate", "Factor1", "Factor2")

  df %>%
    mutate(Date = dmy(DateRaw)) %>%
    mutate(
      BuyRate = as.numeric(BuyRate),
      SellRate = as.numeric(SellRate)
    ) %>%
    filter(Currency == "USD") %>%
    select(Date, LocalExchangeRate = SellRate)
}

local_data <- lapply(files, read_clean_file) %>%
  bind_rows() %>%
  distinct() %>%
  arrange(Date)

# DROP rows after 2003-12-31
local_data <- local_data %>%
  filter(Date <= as.Date("2003-12-31"))

# Save filtered data
write_csv(local_data, "Raw_Data/USD_BRL_exchange_rate_Pre_2003.csv")


##########################################################################################################





####################################Daily log change#############################################################
library(xts)
library(dplyr)
library(readr)
library(quantmod)  # for to.weekly()

# Load your data
coffee_data <- read_csv("Raw_Data/combined_coffee_price_index.csv")
coffee_data$Date <- as.Date(coffee_data$Date)

# Filter to dates with both prices > 0 starting 2001-11-08
coffee_filtered <- coffee_data %>%
  filter(Date >= as.Date("2001-11-08")) %>%
  filter(Price_Arabica > 0, Price_Robusta > 0)

# Create xts objects
arabica_xts <- xts(coffee_filtered$Price_Arabica, order.by = coffee_filtered$Date)
robusta_xts <- xts(coffee_filtered$Price_Robusta, order.by = coffee_filtered$Date)

# Calculate daily log returns
arabica_log_returns <- diff(log(arabica_xts))
robusta_log_returns <- diff(log(robusta_xts))

# Aggregate to weekly log returns using period.apply (sum of daily log returns in the week)
# The endpoint function finds the last day of each week
ep <- endpoints(arabica_log_returns, on = "weeks")

arabica_weekly <- period.apply(arabica_log_returns, INDEX = ep, FUN = sum)
robusta_weekly <- period.apply(robusta_log_returns, INDEX = ep, FUN = sum)

# Plot weekly log returns
par(mfrow = c(2,1), mar = c(4,4,3,2))
plot(na.omit(arabica_weekly),
     main = "Arabica Coffee Weekly Log Returns",
     ylab = "Weekly Log Return",
     col = "darkgreen",
     lwd = 1)

plot(na.omit(robusta_weekly),
     main = "Robusta Coffee Weekly Log Returns",
     ylab = "Weekly Log Return",
     col = "brown",
     lwd = 1)




####################################Daily log change#############################################################



# Example plot with vertical lines

plot(window(arabica_weekly_log_returns,
            start = as.Date("2013-12-01"),
            end = as.Date("2014-06-30")),
     main = "Arabica Coffee Weekly Log Returns with Drought Event (2014)",
     ylab = "Log Returns",
     xlab = "Week",
     col = "darkgreen",
     lwd = 2)

# Add vertical line for drought peak (e.g., Feb 2014)
abline(v = as.Date("2014-02-15"), col = "red", lty = 2, lwd = 2)
text(as.Date("2014-02-15"), 0.1, "Drought Peak", pos = 4, col = "red")




plot(window(arabica_weekly_log_returns,
            start = as.Date("2021-06-01"),
            end = as.Date("2021-09-30")),
     main = "Arabica Coffee Weekly Log Returns with Frost Warning (2021)",
     ylab = "Log Returns",
     xlab = "Week",
     col = "darkgreen",
     lwd = 2)

abline(v = as.Date("2021-07-20"), col = "blue", lty = 2, lwd = 2)
text(as.Date("2021-07-20"), 0.1, "Frost Warning", pos = 4, col = "blue")










