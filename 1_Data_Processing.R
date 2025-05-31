##########################################################################################################
#                           DATA PROCESSING
##########################################################################################################


# Load package
# Load required package
library(rbcb)
library(dplyr)
library(lubridate)

start_year <- 2001
end_year <- year(Sys.Date())

all_data <- lapply(start_year:end_year, function(y) {
  start_date <- as.Date(paste0(y, "-01-01"))
  end_date <- as.Date(paste0(y, "-12-31"))
  if (y == end_year) {
    end_date <- Sys.Date()
  }
  
  message("Downloading data for year: ", y)
  
  # Safely try to get series and handle possible errors
  tryCatch({
    df <- get_series(1, start_date, end_date)
    df
  }, error = function(e) {
    message("Failed for year ", y, ": ", e$message)
    NULL
  })
})

# Remove NULL results if any failed
all_data <- Filter(Negate(is.null), all_data)

# Combine all year data into one dataframe
ptax_data <- bind_rows(all_data) %>%
  rename(PTAX = `1`) %>%
  arrange(date)

# Save to CSV
write.csv(ptax_data, "Raw_Data/USD_BRL_Exchange_Rate.csv", row.names = FALSE)

head(ptax_data)
##########################################################################################################


################################################COFFE PRICE###############################################
# Load required libraries
library(readxl)
library(dplyr)
library(lubridate)

# --- Load raw Excel files ---
# The skip = 3 is neccesary as the value start from the 3rd row
arabica_raw <- read_excel("Raw_Data/Coffee_Data/Arabica Coffe Price Index.xlsx", skip = 3)
robusta_raw <- read_excel("Raw_Data/Coffee_Data/Robusta Coffe Price Index.xlsx", skip = 3)

# --- Clean Arabica ---
arabica <- arabica_raw %>%
  filter(!is.na(Date), !is.na(`Price US$`)) %>%
  mutate(Date = mdy(Date)) %>%  # Use dmy() if date format is dd/mm/yyyy
  distinct(Date, .keep_all = TRUE) %>%
  rename(Price_Arabica = `Price US$`)

# --- Clean Robusta ---
robusta <- robusta_raw %>%
  filter(!is.na(Date), !is.na(`Price US$`)) %>%
  mutate(Date = mdy(Date)) %>%
  distinct(Date, .keep_all = TRUE) %>%
  rename(Price_Robusta = `Price US$`)

# --- Join on matching dates only ---
combined_df <- inner_join(arabica, robusta, by = "Date")

# --- Save the final merged dataset to CSV ---
write.csv(combined_df, "Raw_Data/combined_coffee_price_index.csv", row.names = FALSE)

# --- Print result summary ---
cat("✅ Success! Merged file with", nrow(combined_df), "matched dates saved as 'combined_coffee_price_index.csv'\n")


# Read combined CSV
combined_df <- read.csv("Raw_Data/combined_coffee_price_index.csv", stringsAsFactors = FALSE)
















################################################COFFE PRICE###############################################

# Load required libraries
library(readr)
library(dplyr)
library(xts)
library(quantmod)

# --- Step 1: Load and Prepare Coffee Data ---

# Load coffee price data
coffee_data <- read_csv("Raw_Data/Coffee_Data/combined_coffee_price_index.csv")

# Convert Date column to Date class
coffee_data$Date <- as.Date(coffee_data$Date)

# Filter data: start from 2001-11-08, prices > 0
coffee_filtered <- coffee_data %>%
  filter(Date >= as.Date("2001-11-08")) %>%
  filter(Price_Arabica > 0, Price_Robusta > 0)

# Convert to xts objects
arabica_xts <- xts(coffee_filtered$Price_Arabica, order.by = coffee_filtered$Date)
robusta_xts <- xts(coffee_filtered$Price_Robusta, order.by = coffee_filtered$Date)

# Aggregate to weekly data (closing price on last trading day of week)
arabica_weekly <- to.weekly(arabica_xts, indexAt = "lastof", OHLC = FALSE)
robusta_weekly <- to.weekly(robusta_xts, indexAt = "lastof", OHLC = FALSE)

# --- Step 2: Download and Prepare Exchange Rate Data ---

# Download USD to BRL exchange rate (note: 1 USD = X BRL)
getSymbols("USDBRL=X", src = "yahoo", from = "2001-11-08", auto.assign = TRUE)

# Extract Adjusted Close price
brl_usd <- `USDBRL=X`[, "USDBRL=X.Adjusted"]

# Aggregate exchange rate to weekly frequency matching coffee data
brl_usd_weekly <- to.weekly(brl_usd, indexAt = "lastof", OHLC = FALSE)





# --- Step 3: Calculate Weekly Log Returns ---

arabica_weekly_log_returns <- diff(log(arabica_weekly))
robusta_weekly_log_returns <- diff(log(robusta_weekly))
brl_usd_weekly_log_returns <- diff(log(brl_usd_weekly))

# --- Step 4: Merge the Three Series ---

combined_data <- merge(arabica_weekly_log_returns, robusta_weekly_log_returns, brl_usd_weekly_log_returns)

# Rename columns for clarity
colnames(combined_data) <- c("Arabica_Returns", "Robusta_Returns", "BRL_USD_Returns")

# Remove rows with NA (due to differencing)
combined_data <- na.omit(combined_data)

# --- Step 5: Check the combined data ---

head(combined_data)

# Now 'combined_data' is ready for VAR modeling or further analysis