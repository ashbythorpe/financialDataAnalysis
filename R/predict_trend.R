tbl_to_ts <- function(x) {
  period <- timetk::tk_index(x) %>%
    unique() %>%
    sort() %>%
    timetk::tk_get_frequency(period = "auto", message = FALSE)
  stats::ts(x$price_adjusted, frequency = period)
}

predict_trend_daily <- function(x) {
  ticker <- x$ticker[1]
  if(all(x$ref_date <= max(daily_trend_training$ref_date))) {
    dplyr::left_join(x, daily_trend_training) %>%
      dplyr::pull(am)
  } else if(any(x$ref_date <= max(daily_trend_training$ref_date))) {
    trained <- dplyr::left_join(x, daily_trend_training) %>%
      dplyr::pull(am)
    horizon <- lubridate::interval(
      max(x$ref_date), 
      max(daily_trend_training$ref_date)
    ) %>%
      as.integer()
    model <- dplyr::filter(daily_trend_model, ticker == ticker)$fit[[1]]
    forecasted <- forecast::forecast(model, h = horizon)$mean
    c(trained, forecasted)
  } else {
    horizon <- lubridate::interval(
      max(x$ref_date), 
      max(daily_trend_training$ref_date)
    ) %>%
      as.integer()
    length <- nrow(x)
    model <- dplyr::filter(daily_trend_model, ticker == ticker)$fit[[1]]
    f <- forecast::forecast(model, h = horizon)$mean
    f[(length(f) - length + 1):length(f)]
  }
}

predict_trend_monthly <- function(x) {
  # Make sure the dates match up correctly.
  x$ref_date <- lubridate::round_date(x$ref_date, "month")
  monthly_trend_training$ref_date  <- 
    lubridate::round_date(monthly_trend_training$ref_date, "month")
  # Get the specified ticker
  ticker <- x$ticker[1]
  if(all(x$ref_date <= max(monthly_trend_training$ref_date))) {
    dplyr::left_join(x, monthly_trend_training) %>%
      dplyr::pull(am)
  } else if(any(x$ref_date <= max(monthly_trend_training$ref_date))) {
    trained <- dplyr::left_join(x, monthly_trend_training) %>%
      dplyr::pull(am)
    horizon <- lubridate::interval(max(x$ref_date), 
                                   max(monthly_trend_training$ref_date)) %/% 
      months(1)
    model <- dplyr::filter(monthly_trend_model, ticker == ticker)$fit[[1]]
    forecasted <- forecast::forecast(model, h = horizon)$mean
    c(trained, forecasted)
  } else {
    horizon <- lubridate::interval(max(x$ref_date), 
                                   max(monthly_trend_training$ref_date)) %/% 
      months(1)
    length <- nrow(x)
    model <- dplyr::filter(monthly_trend_model, ticker == ticker)$fit[[1]]
    f <- forecast::forecast(model, h = horizon)$mean
    f[(length(f) - length + 1):length(f)]
  }
}
