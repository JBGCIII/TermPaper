##########################################################################################################
#                                         DATA SET CREATION FULLL
##########################################################################################################

# Load or install required packages
required_packages <- c("readxl", "dplyr", "lubridate", "rbcb", "nasapower", "quantmod", "zoo","xts" )

installed <- required_packages %in% installed.packages()
if (any(!installed)) {
  install.packages(required_packages[!installed])
}
invisible(lapply(required_packages, library, character.only = TRUE))

# Create necessary directories
dir.create("Raw_Data/Coffee_Data", recursive = TRUE, showWarnings = FALSE)
dir.create("Raw_Data/Exchange_Rate", recursive = TRUE, showWarnings = FALSE)
dir.create("Raw_Data/Weather_Data", recursive = TRUE, showWarnings = FALSE)

##########################################################################################################
###                               1. Arabica and Robusta Prices                                        ### 
##########################################################################################################

arabica_raw <- read_excel("Raw_Data/Coffee_Data/Arabica Coffe Price Index.xlsx", skip = 3)
robusta_raw <- read_excel("Raw_Data/Coffee_Data/Robusta Coffe Price Index.xlsx", skip = 3)

arabica <- arabica_raw %>%
  filter(!is.na(Date), !is.na(`Price US$`)) %>%
  mutate(Date = mdy(Date)) %>%
  distinct(Date, .keep_all = TRUE) %>%
  rename(Price_Arabica = `Price US$`)

robusta <- robusta_raw %>%
  filter(!is.na(Date), !is.na(`Price US$`)) %>%
  mutate(Date = mdy(Date)) %>%
  distinct(Date, .keep_all = TRUE) %>%
  rename(Price_Robusta = `Price US$`)

combined_df <- inner_join(arabica, robusta, by = "Date") %>%
  filter(Date >= as.Date("2001-11-08"))

write.csv(combined_df, "Raw_Data/Coffee_Data/combined_coffee_price_index.csv", row.names = FALSE)

##########################################################################################################
###                               2. Exchange Rate (PTAX)                                              ### 
##########################################################################################################

start_year <- 2001
end_year <- year(Sys.Date())

all_data <- lapply(start_year:end_year, function(y) {
  start_date <- as.Date(paste0(y, "-01-01"))
  end_date_temp <- if (y == end_year) Sys.Date() else as.Date(paste0(y, "-12-31"))
  tryCatch({
    get_series(1, start_date, end_date_temp)
  }, error = function(e) {
    message("Failed for year ", y, ": ", e$message)
    NULL
  })
})

all_data <- Filter(Negate(is.null), all_data)
ptax_data <- bind_rows(all_data) %>%
  rename(PTAX = `1`) %>%
  arrange(date)

write.csv(ptax_data, "Raw_Data/Exchange_Rate/USD_BRL_Exchange_Rate.csv", row.names = FALSE)

##########################################################################################################
###                               3. Arabica Futures from Yahoo                                        ### 
##########################################################################################################

# Define dates
start_date <- as.Date("2001-01-01")
end_date <- as.Date("2025-05-29")

# Download KC=F futures, suppress warnings about missing data
suppressWarnings(getSymbols("KC=F", src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE))

# Convert to data.frame
arabica_df <- data.frame(
  Date = index(`KC=F`),
  Close = as.numeric(Cl(`KC=F`))
)

# Add Close_USD_60kg column *without* using select() separately
arabica_df$Close_USD_60kg <- arabica_df$Close * 0.01 * 132.277

# Keep only Date and Close_USD_60kg in a new data frame (base R subsetting)
arabica_clean <- arabica_df[, c("Date", "Close_USD_60kg")]

arabica_clean <- na.omit(arabica_clean)

# Create directory if it doesn't exist
dir.create("Raw_Data/Coffee_Data", recursive = TRUE, showWarnings = FALSE)

# Write CSV
write.csv(arabica_clean, "Raw_Data/Coffee_Data/Arabica_Futures_Close_USD_60kg.csv", row.names = FALSE)

##########################################################################################################
###                               4. Weather Data (NASA)                                               ### 
##########################################################################################################

# Ensure the parent directory exists
dir.create("Raw_Data/Weather_Data", recursive = TRUE, showWarnings = FALSE)

# Define the location (longitude and latitude) — example: Minas Gerais, Brazil
lon <- -45.43
lat <- -21.55

# Define date range
start_date <- as.Date("2001-11-08")
end_date <- as.Date("2025-05-29")

# Retrieve weather data using nasapower
weather_full <- get_power(
  community = "AG",  # Agricultural data
  pars = c("T2M_MAX", "T2M_MIN", "RH2M", "ALLSKY_SFC_SW_DWN", "PRECTOTCORR"),
  dates = c(start_date, end_date),
  temporal_average = "DAILY",
  lonlat = c(lon, lat)
)

# Convert to data.frame to avoid class issues
weather_full <- as.data.frame(weather_full)

# Select and rename relevant columns
weather_clean <- weather_full %>%
  dplyr::select(Date, T2M_MAX, T2M_MIN, RH2M, ALLSKY_SFC_SW_DWN, PRECTOTCORR) %>%
  dplyr::rename(
    Temp_Max = T2M_MAX,
    Temp_Min = T2M_MIN,
    Humidity = RH2M,
    Solar_Radiation = ALLSKY_SFC_SW_DWN,
    Precipitation_mm = PRECTOTCORR
  )

# Write to CSV
print(getwd())
print("Attempting to write CSV...")

write.csv(weather_clean, "Raw_Data/Weather_Data/weather.csv", row.names = FALSE)
print("CSV file written.")
##########################################################################################################
###                               5. Merge All Datasets                                                ### 
##########################################################################################################

# Load datasets
coffee_data <- read.csv("Raw_Data/Coffee_Data/combined_coffee_price_index.csv") %>%
  mutate(Date = as.Date(Date))

arabica_simple <- read.csv("Raw_Data/Coffee_Data/Arabica_Futures_Close_USD_60kg.csv") %>%
  mutate(Date = as.Date(Date))

ptax_data <- read.csv("Raw_Data/Exchange_Rate/USD_BRL_Exchange_Rate.csv") %>%
  mutate(Date = as.Date(date)) %>%
  dplyr::select(Date, PTAX)

weather_data <- read.csv("Raw_Data/Weather_Data/weather.csv") %>%
  mutate(Date = as.Date(Date))

# Merge all datasets by Date
merged_data <- ptax_data %>%
  inner_join(coffee_data, by = "Date") %>%
  inner_join(arabica_simple, by = "Date") %>%
  inner_join(weather_data, by = "Date") %>%
  filter(Date >= as.Date("2001-11-08")) %>%
  arrange(Date)

# Save merged data
write.csv(merged_data, "Raw_Data/Coffee_Data_Set.csv", row.names = FALSE)




