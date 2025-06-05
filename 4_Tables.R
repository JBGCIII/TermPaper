##########################################################################################################
#                                   PRE-VAR ANALYSIS GRAPHICAL
##########################################################################################################

library(urca)
library(dplyr)
library(tidyr)

spot_vec <- ts(rnorm(100))
futures_vec <- ts(rnorm(100))
log_spot <- log(spot_vec)
log_futures <- log(futures_vec)
diff_log_spot <- diff(log_spot)
diff_log_futures <- diff(log_futures)

# --- Function to run ADF test and extract key info ---
extract_adf_results <- function(ts_data, series_name) {
  test_types <- c("none", "drift", "trend")

  results_list <- lapply(test_types, function(test_type) {
    adf_test <- ur.df(ts_data, type = test_type, selectlags = "AIC")
    
    # Extract test statistic (tau) — first test stat
    test_stat <- adf_test@teststat[1]
    
    # Extract critical values (named vector)
    crit_vals <- adf_test@cval[1,]
    
    tibble(
      Series = series_name,
      Test_Type = test_type,
      Test_Statistic = test_stat,
      Critical_1pct = crit_vals["1pct"],
      Critical_5pct = crit_vals["5pct"],
      Critical_10pct = crit_vals["10pct"],
    )
  })

  bind_rows(results_list)
}

# --- Prepare series list with only existing objects ---
series_names <- c("spot_vec", "futures_vec", "log_spot", "log_futures", "diff_log_spot", "diff_log_futures")
existing_series <- series_names[sapply(series_names, exists)]

# Create named list of the existing series objects
series_list <- lapply(existing_series, get)
names(series_list) <- existing_series

# --- Run tests on all series and combine results ---
all_results <- bind_rows(
  lapply(names(series_list), function(name) {
    extract_adf_results(series_list[[name]], series_name = name)
  })
)

# --- Save results to CSV ---
write.csv(all_results, "Processed_Data/ADF_Test_Results.csv", row.names = FALSE)
