predict_residuals_daily <- function(stock, dates, preds, hostess, r) {
  data <- dplyr::filter(daily_training_data, ticker == stock) %>%
    dplyr::rename(ds = "ref_date")
  
  horizon <- lubridate::interval(
    max(data$ds),
    max(dates)
  ) %/% lubridate::days(1)
  
  # Forecast from the end of the training data to the end of the specified 
  # period
  forecast_stock_daily(
    get_lightgbm_model("daily"),
    stock = stock,
    horizon = horizon,
    training_data = data,
    hostess = hostess,
    r = r
  ) %>%
    dplyr::filter(ds %in% dates) %>%
    dplyr::pull(residuals)
}

predict_residuals_monthly <- function(stock, dates, preds, hostess, r) {
  # Make sure dates line up correctly
  data <- dplyr::filter(monthly_training_data, ticker == stock) %>%
    dplyr::rename(ds = "ref_date")
  
  horizon <- lubridate::interval(
    max(data$ds),
    max(dates)
  ) %/% months(1) # Get the number of months in the interval
  
  # Forecast from the end of the training data to the end of the specified 
  # period
  forecast_stock_monthly(
    get_lightgbm_model("monthly"),
    stock = stock,
    horizon = horizon,
    training_data = data,
    hostess = hostess,
    r = r
  ) %>%
    dplyr::filter(ds %in% dates) %>% # Extract the relevant period
    dplyr::pull(residuals)
}
