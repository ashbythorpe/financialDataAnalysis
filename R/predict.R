predict_price <- function(stock, start_date = lubridate::today(),
                          end_date = lubridate::today() + lubridate::years(1),
                          freq = c("daily", "monthly")){
  if(is.null(freq) || all(!freq %in% c("daily", "monthly"))) {
    return(NULL)
  }
  freq <- rlang::arg_match(freq)
  
  if(is.null(stock) || is.null(start_date) || is.null(end_date) ||
     start_date > end_date){
    return(NULL)
  }
  
  if(freq == "daily") {
    dates <- seq(start_date, end_date, by = "day")
    
    pred <- predict_price_daily(stock, dates)
  } else {
    dates <- seq(start_date, end_date, by = "month")
    
    pred <- predict_price_monthly(stock, dates)
  }
  tibble::tibble(
    ref_date = dates,
    .pred = pred
  )
}

predict_price_daily <- function(stock, dates) {
  data <- dplyr::filter(daily_stock_data, ticker == stock)
  
  if(max(dates) <= max(daily_stock_data$ref_date)) {
    dplyr::left_join(
      tibble::tibble(ref_date = dates),
      data
    ) %>%
      dplyr::pull(price_adjusted)
  } else if(min(dates) <= max(daily_stock_data$ref_date)) {
    dates_before <- dates[dates <= max(daily_stock_data$ref_date)]
    dates_after <- dates[dates > max(daily_stock_data$ref_date)]
    
    known_prices <- dplyr::left_join(
      tibble::tibble(ref_date = dates_before),
      data
    ) %>%
      dplyr::pull(price_adjusted)
    
    prophet_model <- dplyr::filter(daily_prophet_model, ticker == stock)$fit[[1]]
    pred_data <- tibble::tibble(ds = dates_after)
    
    preds <- withr::with_package("prophet", {
      predict(prophet_model, pred_data) %>%
        dplyr::select(ds, yhat) %>%
        dplyr::mutate(ds = lubridate::date(ds))
    })
    
    residuals <- predict_residuals_daily(stock, dates_after, preds)
    
    c(known_prices, preds$yhat + residuals)
  } else {
    prophet_model <- dplyr::filter(daily_prophet_model, ticker == stock)$fit[[1]]
    pred_data <- tibble::tibble(ds = dates)
    
    preds <- withr::with_package("prophet", {
      predict(prophet_model, pred_data) %>%
        dplyr::select(ds, yhat) %>%
        dplyr::mutate(ds = lubridate::date(ds))
    })
    
    residuals <- predict_residuals_daily(stock, dates, preds)
    
    preds$yhat + residuals
  }
}

predict_price_monthly <- function(stock, dates) {
  # Make sure dates line up correctly
  dates <- lubridate::round_date(dates, "month")
  data <- dplyr::filter(monthly_stock_data, ticker == stock) %>%
    dplyr::mutate(ref_date = lubridate::round_date(ref_date, "month"))
  
  if(max(dates) <= max(monthly_stock_data$ref_date)) {
    dplyr::left_join(
      tibble::tibble(ref_date = dates),
      data
    ) %>%
      dplyr::pull(price_adjusted)
  } else if(min(dates) <= max(monthly_stock_data$ref_date)) {
    dates_before <- dates[dates <= max(monthly_stock_data$ref_date)]
    dates_after <- dates[dates > max(monthly_stock_data$ref_date)]
    
    known_prices <- dplyr::left_join(
      tibble::tibble(ref_date = dates_before),
      data
    ) %>%
      dplyr::pull(price_adjusted)
    
    prophet_model <- dplyr::filter(monthly_prophet_model, ticker == stock)$fit[[1]]
    pred_data <- tibble::tibble(ds = dates_after)
    
    preds <- withr::with_package("prophet", {
      predict(prophet_model, pred_data) %>%
        dplyr::select(ds, yhat) %>%
        dplyr::mutate(ds = lubridate::date(ds))
    })
    
    residuals <- predict_residuals_monthly(stock, dates_after, preds)
    
    c(known_prices, preds$yhat + residuals)
  } else {
    prophet_model <- dplyr::filter(monthly_prophet_model, ticker == stock)$fit[[1]]
    pred_data <- tibble::tibble(ds = dates)
    
    preds <- withr::with_package("prophet", {
      predict(prophet_model, pred_data) %>%
        dplyr::select(ds, yhat) %>%
        dplyr::mutate(ds = lubridate::date(ds))
    })
    
    residuals <- predict_residuals_monthly(stock, dates, preds)
    
    preds$yhat + residuals
  }
}

plot_predictions <- function(predicted){
  if(is.null(predicted)){
    return(NULL)
  }
  ggplot2::ggplot(predicted, ggplot2::aes(x = ref_date, y = .pred)) +
    ggplot2::geom_line()
}

