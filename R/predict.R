#' Predict the price of a stock over time
#' 
#' Generate predictions for the price of a stock over a specified time period.
#' Choose between daily or monthly predictions.
#' 
#' @param stock The stock to generate predictions for (e.g. "GOOGL").
#' @param start_date The first date of the predictions.
#' @param end_date The final date of the predictions.
#' @param freq the frequency of the predictions.
#' 
#' @details 
#' The prediction process involves using a Prophet model to generate 
#' predictions, then using a lightgbm model to predict the residuals and adding
#' the two together.
#' 
#' @returns A [tibble::tibble()] of stock predictions.
#' 
#' @seealso 
#' * [plot_predictions()] to plot your generated predictions.
#' * [prophet_models] and [get_lightgbm_model()] for the underlying models.
#' 
#' @export
predict_price <- function(stock, start_date = lubridate::today(),
                          end_date = lubridate::today() + lubridate::days(60),
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
  data <- dplyr::filter(daily_training_data, ticker == stock)
  
  if(max(dates) <= max(daily_training_data$ref_date)) {
    dplyr::left_join(
      tibble::tibble(ref_date = dates),
      data
    ) %>%
      dplyr::pull(price_adjusted)
  } else if(min(dates) <= max(daily_training_data$ref_date)) {
    dates_before <- dates[dates <= max(daily_training_data$ref_date)]
    dates_after <- dates[dates > max(daily_training_data$ref_date)]
    
    known_prices <- dplyr::left_join(
      tibble::tibble(ref_date = dates_before),
      data
    ) %>%
      dplyr::pull(price_adjusted)
    
    prophet_model <- dplyr::filter(daily_prophet_model, ticker == stock)$fit[[1]]
    pred_data <- tibble::tibble(ds = dates_after)
    
    preds <- withr::with_package("prophet", {
      stats::predict(prophet_model, pred_data) %>%
        dplyr::select(ds, yhat) %>%
        dplyr::mutate(ds = lubridate::date(ds))
    })
    
    residuals <- withr::with_package("workflows", {
      predict_residuals_daily(stock, dates_after, preds)
    })
    
    c(known_prices, preds$yhat + residuals)
  } else {
    prophet_model <- dplyr::filter(daily_prophet_model, ticker == stock)$fit[[1]]
    pred_data <- tibble::tibble(ds = dates)
    
    preds <- withr::with_package("prophet", {
      stats::predict(prophet_model, pred_data) %>%
        dplyr::select(ds, yhat) %>%
        dplyr::mutate(ds = lubridate::date(ds))
    })
    
    residuals <- withr::with_package("workflows", {
      predict_residuals_daily(stock, dates, preds)
    })
    
    preds$yhat + residuals
  }
}

predict_price_monthly <- function(stock, dates) {
  # Make sure dates line up correctly
  dates <- lubridate::round_date(dates, "month")
  data <- dplyr::filter(monthly_training_data, ticker == stock) %>%
    dplyr::mutate(ref_date = lubridate::round_date(ref_date, "month"))
  
  if(max(dates) <= max(monthly_training_data$ref_date)) {
    dplyr::left_join(
      tibble::tibble(ref_date = dates),
      data
    ) %>%
      dplyr::pull(price_adjusted)
  } else if(min(dates) <= max(monthly_training_data$ref_date)) {
    dates_before <- dates[dates <= max(monthly_training_data$ref_date)]
    dates_after <- dates[dates > max(monthly_training_data$ref_date)]
    
    known_prices <- dplyr::left_join(
      tibble::tibble(ref_date = dates_before),
      data
    ) %>%
      dplyr::pull(price_adjusted)
    
    prophet_model <- dplyr::filter(monthly_prophet_model, ticker == stock)$fit[[1]]
    pred_data <- tibble::tibble(ds = dates_after)
    
    preds <- withr::with_package("prophet", {
      stats::predict(prophet_model, pred_data) %>%
        dplyr::select(ds, yhat) %>%
        dplyr::mutate(ds = lubridate::date(ds))
    })
    
    residuals <- predict_residuals_monthly(stock, dates_after, preds)
    
    c(known_prices, preds$yhat + residuals)
  } else {
    prophet_model <- dplyr::filter(monthly_prophet_model, ticker == stock)$fit[[1]]
    pred_data <- tibble::tibble(ds = dates)
    
    preds <- withr::with_package("prophet", {
      stats::predict(prophet_model, pred_data) %>%
        dplyr::select(ds, yhat) %>%
        dplyr::mutate(ds = lubridate::date(ds))
    })
    
    residuals <- predict_residuals_monthly(stock, dates, preds)
    
    preds$yhat + residuals
  }
}

#' Plot price predictions
#' 
#' Take a set of predictions and generate a line plot of them.
#' 
#' @param predicted A data frame of predictions, likely generated by the
#'   [predict_price()] function.
#'   
#' @returns A [ggplot2::ggplot()] object.
#' 
#' @seealso [predict_price()]
#' 
#' @export
plot_predictions <- function(predicted){
  if(is.null(predicted)){
    return(NULL)
  }
  ggplot2::ggplot(na.omit(predicted), ggplot2::aes(x = ref_date, y = .pred)) +
    ggplot2::geom_line()
}

