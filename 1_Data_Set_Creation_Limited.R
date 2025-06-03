##########################################################################################################
#                                         DATA SET CREATION LIMITED
##########################################################################################################
# This script allows to create the limited data set which focuses on the key varaibles for the analysis


# Vector of required packages
required_packages <- c("quantmod", "readxl", "dplyr", "lubridate")

# Install any packages that are not already installed
installed <- required_packages %in% installed.packages()
if (any(!installed)) {
  install.packages(required_packages[!installed])
}

# Load all packages
invisible(lapply(required_packages, library, character.only = TRUE))

################################################COFFE PRICE###############################################

# Load raw Excel files ---
# The skip = 3 is neccesary as the value start from the 3rd row
arabica_raw <- read_excel("Raw_Data/Coffee_Data/Arabica Coffe Price Index.xlsx", skip = 3)
robusta_raw <- read_excel("Raw_Data/Coffee_Data/Robusta Coffe Price Index.xlsx", skip = 3)

# Clean Arabica ---
arabica <- arabica_raw %>%
  filter(!is.na(Date), !is.na(`Price US$`)) %>%
  mutate(Date = mdy(Date)) %>%  # Use dmy() if date format is dd/mm/yyyy
  distinct(Date, .keep_all = TRUE) %>%
  rename(Price_Arabica = `Price US$`)

# Clean Robusta ---
robusta <- robusta_raw %>%
  filter(!is.na(Date), !is.na(`Price US$`)) %>%
  mutate(Date = mdy(Date)) %>%
  distinct(Date, .keep_all = TRUE) %>%
  rename(Price_Robusta = `Price US$`)

# Join on matching dates only ---
combined_df <- inner_join(arabica, robusta, by = "Date")

# Save the final merged dataset to CSV ---
write.csv(combined_df, "Raw_Data/Coffee_Data/combined_coffee_price_index.csv", row.names = FALSE)

##########################################COFFE FUTURES##############################################

# Define date range
start_date <- as.Date("2001-01-01")
end_date <- Sys.Date()

# Download Arabica futures data (ticker: KC=F)
getSymbols("KC=F", src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)

# Extract the data object
arabica_xts <- `KC=F`

# Remove rows with NA in Close price
arabica_xts <- na.omit(arabica_xts)

# Extract Date and Close price (4th column)
arabica_df <- data.frame(
  Date = index(arabica_xts),
  Close = as.numeric(arabica_xts[, "KC=F.Close"])
)

# Convert Close price from cents/lb to USD per 60-kg bag
arabica_df$Close_USD_60kg <- arabica_df$Close * 0.01 * 132.277

# Keep only Date and converted Close
arabica_simple <- arabica_df[, c("Date", "Close_USD_60kg")]

# Save to CSV
write.csv(arabica_simple, "Raw_Data/Coffee_Data/Arabica_Futures_Close_USD_60kg.csv", row.names = FALSE)


############################################## COMBINING DATA SET #########################################

# 1. Read the processed datasets
coffee_data <- read.csv("Raw_Data/Coffee_Data/combined_coffee_price_index.csv", stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date)) %>%
  select(Date, Price_Arabica, Price_Robusta)

arabica_simple <- read.csv("Raw_Data/Coffee_Data/Arabica_Futures_Close_USD_60kg.csv", stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date))  # Ensure Date is properly parsed

# 2. Merge datasets by Date (inner join to keep only dates present in both datasets)
merged_data <- inner_join(coffee_data, arabica_simple, by = "Date") %>%
  arrange(Date)

# 3. Remove rows before 2001-11-08
merged_data <- merged_data %>% 
  filter(Date >= as.Date("2001-11-08"))

# 4. Check the first few rows
head(merged_data)

# 5. Save combined dataset
write.csv(merged_data, "Raw_Data/Coffee_Data_Set.csv", row.names = FALSE)





