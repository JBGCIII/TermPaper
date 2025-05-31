



arabica_ts <- ts(coffee_data$arabica, start = c(2001, 1), frequency = 365)
plot(arabica_ts,
     main = "Arabica Coffee Prices",
     ylab = "Price (USD/lb)",
     xlab = "Time",
     lwd = 2,
     col = "darkgreen")