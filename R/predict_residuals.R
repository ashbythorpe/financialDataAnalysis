predict_residuals_daily <- function(stock, dates, preds) {
  data <- dplyr::filter(daily_stock_data, ticker == stock) %>%
    dplyr::rename(ds = "ref_date") %>%
    dplyr::left_join(preds, by = "ds") %>%
    dplyr::mutate(residuals = price_adjusted - yhat)
  
  horizon <- lubridate::interval(
    max(data$ds),
    max(dates)
  ) %/% lubridate::days(1)
  
  # Forecast from the end of the training data to the end of the specified 
  # period
  forecast_stock_daily(
    daily_lightgbm_model,
    stock = stock,
    horizon = horizon,
    training_data = data
  ) %>%
    dplyr::filter(ds %in% dates) %>%
    dplyr::pull(residuals)
}

predict_residuals_monthly <- function(stock, dates, preds) {
  # Make sure dates line up correctly
  data <- dplyr::filter(monthly_stock_data, ticker == stock) %>%
    dplyr::rename(ds = "ref_date") %>%
    dplyr::left_join(preds, by = "ds") %>%
    dplyr::mutate(residuals = price_adjusted - yhat)
  
  horizon <- lubridate::interval(
    max(data$ds),
    max(dates)
  ) %/% months(1) # Get the number of months in the interval
  
  # Forecast from the end of the training data to the end of the specified 
  # period
  forecast_stock_monthly(
    monthly_lightgbm_model,
    stock = stock,
    horizon = horizon,
    training_data = data
  ) %>%
    dplyr::filter(ds %in% dates) %>% # Extract the relevant period
    dplyr::pull(residuals)
}
