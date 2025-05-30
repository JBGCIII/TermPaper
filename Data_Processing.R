
library(readxl)
library(dplyr)

# Read the Excel files
arabica <- read_excel("path/to/Arabica Coffe Price Index.xls")
robusta <- read_excel("path/to/Robusta Coffe Price Index.xls")

# Convert Date columns to Date type (adjust column names if different)
arabica$Date <- as.Date(arabica$Date)
robusta$Date <- as.Date(robusta$Date)

# Inner join on Date to keep only matching dates
combined_df <- inner_join(arabica, robusta, by = "Date")

cat("Number of matched daily records:", nrow(combined_df), "\n")

# Save combined data frame as a new CSV file
write.csv(combined_df, "combined_coffee_price_index.csv", row.names = FALSE)

cat("Combined data saved as combined_coffee_price_index.csv\n")