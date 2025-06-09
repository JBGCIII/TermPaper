##########################################################################################################
#                                         DATA SET CREATION FULLL
##########################################################################################################

# Load or install required packages
required_packages <- c("readxl", "dplyr", "lubridate", "rbcb", "nasapower", "quantmod")

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


# 1. Download Arabica futures data
start_date <- as.Date("2001-01-01")
end_date <- as.Date("2025-05-29")

getSymbols("KC=F", src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)

# 2. Convert to data.frame immediately
arabica_df <- data.frame(
  Date = index(`KC=F`),
  Close = as.numeric(Cl(`KC=F`))
)

# 3. Remove NAs and compute Close_USD_60kg
transmute(Date, Close_USD_60kg = Close * 0.01 * 132.277)

# 4. Save to CSV
dir.create("Raw_Data/Coffee_Data", recursive = TRUE, showWarnings = FALSE)
write.csv(arabica_clean, "Raw_Data/Coffee_Data/Arabica_Futures_Close_USD_60kg.csv", row.names = FALSE)

##########################################################################################################
###                               4. Weather Data (NASA)                                               ### 
##########################################################################################################

lon <- -45.43
lat <- -21.55

weather_ag <- get_power(
  community = "ag",
  lonlat = c(lon, lat),
  pars = c("T2M_MAX", "T2M_MIN", "RH2M", "ALLSKY_SFC_SW_DWN"),
  temporal_api = "DAILY",
  dates = c(start_date, end_date)
)

weather_re <- get_power(
  community = "re",
  lonlat = c(lon, lat),
  pars = c("PRECTOTCORR"),
  temporal_api = "DAILY",
  dates = c(start_date, end_date)
)

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
  select(Date, PTAX)

weather_data <- read.csv("Raw_Data/Weather_Data/weather.csv") %>%
  mutate(Date = as.Date(Date))

# Merge all
merged_data <- ptax_data %>%
  inner_join(coffee_data, by = "Date") %>%
  inner_join(arabica_simple, by = "Date") %>%
  inner_join(weather_data, by = "Date") %>%
  filter(Date >= as.Date("2001-11-08")) %>%
  arrange(Date)

write.csv(merged_data, "Raw_Data/Coffee_Data_Set_test.csv", row.names = FALSE)





