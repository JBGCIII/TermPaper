##########################################################################################################
#                                         DATA PROCESSING
##########################################################################################################

##########################################EXCHANGE-RATE-DATA#################################################
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
write.csv(ptax_data, "Raw_Data/Exchange_Rate/USD_BRL_Exchange_Rate.csv", row.names = FALSE)

head(ptax_data)

##################################################WEATHER ############################################
# Load libraries
library(nasapower)
library(dplyr)
library(lubridate)

lon <- -45.43
lat <- -21.55
start_date <- "2001-01-01"
end_date <- format(Sys.Date(), "%Y-%m-%d")

# Get temp, humidity, solar radiation from "ag" community
weather_ag <- get_power(
  community = "ag",
  lonlat = c(lon, lat),
  pars = c("T2M_MAX", "T2M_MIN", "RH2M", "ALLSKY_SFC_SW_DWN"),
  temporal_api = "DAILY",
  dates = c(start_date, end_date)
)

# Get precipitation from "re" community with PRECTOTCORR
weather_re <- get_power(
  community = "re",
  lonlat = c(lon, lat),
  pars = c("PRECTOTCORR"),
  temporal_api = "DAILY",
  dates = c(start_date, end_date)
)

# Merge datasets on date parts
weather_full <- weather_ag %>%
  inner_join(weather_re, by = c("YEAR", "MM", "DD")) %>%
  mutate(Date = ymd(paste(YEAR, MM, DD, sep = "-"))) %>%
  select(Date, T2M_MAX, T2M_MIN, RH2M, ALLSKY_SFC_SW_DWN, PRECTOTCORR) %>%
  rename(
    Temp_Max = T2M_MAX,
    Temp_Min = T2M_MIN,
    Humidity = RH2M,
    Solar_Radiation = ALLSKY_SFC_SW_DWN,
    Precipitation_mm = PRECTOTCORR
  )

write.csv(weather_full, "Raw_Data/Weather_Data/weather.csv", row.names = FALSE)

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
write.csv(combined_df, "Raw_Data/Coffee_Data/combined_coffee_price_index.csv", row.names = FALSE)


##############################################COMBINING DATA SET#########################################
# Load necessary packages
library(dplyr)
library(lubridate)

# 1. Read the processed datasets
ptax_data <- read.csv("Raw_Data/Exchange_Rate/USD_BRL_Exchange_Rate.csv", stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(date)) %>%
  select(Date, PTAX)

weather_data <- read.csv("Raw_Data/Weather_Data/weather.csv", stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date)) %>%
  select(Date, Temp_Max, Temp_Min, Humidity, Solar_Radiation, Precipitation_mm)

coffee_data <- read.csv("Raw_Data/Coffe_Data/combined_coffee_price_index.csv", stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date)) %>%
  select(Date, Price_Arabica, Price_Robusta)

# 2. Merge all datasets by Date (inner join to keep only dates present in all datasets)
merged_data <- ptax_data %>%
  inner_join(coffee_data, by = "Date") %>%
  inner_join(weather_data, by = "Date") %>%
  arrange(Date)

# 3. Check the first few rows
head(merged_data)

# 4. Save combined dataset
write.csv(merged_data, "Raw_Data/Coffe_Data_Set.csv", row.names = FALSE)


