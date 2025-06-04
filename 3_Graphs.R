##########################################################################################################
#                                   PRE-VAR ANALYSIS GRAPHICAL
##########################################################################################################

#########################################NO TRANSFORMATION################################################

# Load necessary libraries
library(readr)
library(dplyr)
library(xts) # --> Since we are dealing with irregular time series!
library(zoo)

# Read and prepare data
coffee_data <- read_csv("Raw_Data/Coffee_Data_Set.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  arrange(Date)

# Create xts objects indexed by Date
arabica_xts <- xts(coffee_data$Price_Arabica, order.by = coffee_data$Date)
robusta_xts <- xts(coffee_data$Price_Robusta, order.by = coffee_data$Date)
arabica_futures_xts <- xts(coffee_data$Close_USD_60kg, order.by = coffee_data$Date)

# Merge all series into one xts object
merged_prices <- merge(arabica_xts, robusta_xts, arabica_futures_xts)
colnames(merged_prices) <- c("Arabica", "Robusta", "Arabica Futures")

# Save the plot as PNG
png("Processed_Data/graph_1_coffee_prices_with_futures.png", width = 1200, height = 600)

# Plot all series on the same graph
plot.zoo(
  merged_prices,
  plot.type = "single",
  col = c("darkgreen", "brown", "blue"),
  lwd = 0.5,
  ylab = "Price (USD/60kg)",
  xlab = "Date",
  main = "Arabica, Robusta, and Arabica Futures Prices"
)

# Add legend
legend("topright", legend = colnames(merged_prices),
       col = c("darkgreen", "brown", "blue"), lty = 1, lwd = 0.5)

# Close the PNG device
dev.off()

