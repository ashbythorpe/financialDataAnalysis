#' Predict the price of a stock over time
#'
#' Generate predictions for the price of a stock over a specified time period.
#' Choose between daily or monthly predictions.
#'
#' @param stock The stock to generate predictions for (e.g. "GOOGL").
#' @param start_date The first date of the predictions.
#' @param end_date The final date of the predictions.
#' @param freq the frequency of the predictions.
#' @param hostess A loader to show the progress of the predictions. A
#'   [waiter::Hostess] object.
#'
#' @details
#' The prediction process involves using a Prophet model to generate
#' predictions, then using a LightGBM model to predict the residuals and adding
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
                          freq = c("daily", "monthly"),
                          hostess = dummy_hostess()) {
  if (is.null(freq) || all(!freq %in% c("daily", "monthly"))) {
    return(NULL)
  }
  freq <- rlang::arg_match(freq)

  if (is.null(stock) || is.null(start_date) || is.null(end_date) ||
    start_date > end_date) {
    return(NULL)
  }

  if (freq == "daily") {
    dates <- seq(start_date, end_date, by = "day")

    pred <- predict_price_daily(stock, dates, hostess)
  } else {
    dates <- seq(start_date, end_date, by = "month")

    pred <- predict_price_monthly(stock, dates, hostess)
  }

  pred$ref_date <- dates
  pred
}

predict_price_daily <- function(stock, dates, hostess) {
  data <- dplyr::filter(daily_training_data, ticker == stock)

  if (max(dates) <= max(daily_training_data$ref_date)) {
    x <- dplyr::left_join(
      tibble::tibble(ref_date = dates),
      data
    ) %>%
      dplyr::pull(price_adjusted)

    hostess$set(90)

    tibble::tibble(
      yhat = x,
      yhat_lower = x,
      yhat_upper = x
    )
  } else if (min(dates) <= max(daily_training_data$ref_date)) {
    dates_before <- dates[dates <= max(daily_training_data$ref_date)]
    dates_after <- dates[dates > max(daily_training_data$ref_date)]

    known_prices <- dplyr::left_join(
      tibble::tibble(ref_date = dates_before),
      data
    ) %>%
      dplyr::pull(price_adjusted)

    known_data <- tibble::tibble(
      yhat = known_prices,
      yhat_lower = known_prices,
      yhat_upper = known_prices
    )

    hostess$set(5)

    r <- get_model_ratio_daily(dates_after)
    prophet_r <- r * 0.85
    lightgbm_r <- (1 - r) * 0.85

    preds <- predict_with_cache_daily(stock, dates_after)

    hostess$inc(unname(prophet_r * 100))

    residuals <- predict_residuals_daily(
      stock, dates_after, hostess,
      lightgbm_r
    )

    preds$yhat <- preds$yhat + residuals
    dplyr::bind_rows(known_data, preds)
  } else {
    r <- get_model_ratio_daily(dates)
    prophet_r <- r * 0.9
    lightgbm_r <- (1 - r) * 0.9

    preds <- predict_with_cache_daily(stock, dates)

    hostess$set(unname(prophet_r * 100))

    residuals <- predict_residuals_daily(
      stock, dates, hostess,
      lightgbm_r
    )

    preds$yhat <- preds$yhat + residuals
    preds
  }
}

predict_price_monthly <- function(stock, dates, hostess) {
  # Make sure dates line up correctly
  dates <- lubridate::round_date(dates, "month")
  data <- dplyr::filter(monthly_training_data, ticker == stock) %>%
    dplyr::mutate(ref_date = lubridate::round_date(ref_date, "month"))

  if (max(dates) <= max(monthly_training_data$ref_date)) {
    x <- dplyr::left_join(
      tibble::tibble(ref_date = dates),
      data
    ) %>%
      dplyr::pull(price_adjusted)

    hostess$set(90)

    tibble::tibble(
      yhat = x,
      yhat_lower = x,
      yhat_upper = x
    )
  } else if (min(dates) <= max(monthly_training_data$ref_date)) {
    dates_before <- dates[dates <= max(monthly_training_data$ref_date)]
    dates_after <- dates[dates > max(monthly_training_data$ref_date)]

    known_prices <- dplyr::left_join(
      tibble::tibble(ref_date = dates_before),
      data
    ) %>%
      dplyr::pull(price_adjusted)

    known_data <- tibble::tibble(
      yhat = known_prices,
      yhat_lower = known_prices,
      yhat_upper = known_prices
    )

    hostess$set(5)

    r <- get_model_ratio_monthly(dates_after)
    prophet_r <- r * 0.85
    lightgbm_r <- (1 - r) * 0.85

    preds <- predict_with_cache_monthly(stock, dates_after)

    hostess$inc(unname(prophet_r * 100))

    residuals <- predict_residuals_monthly(
      stock, dates_after, hostess,
      lightgbm_r
    )

    preds$yhat <- preds$yhat + residuals
    dplyr::bind_rows(known_data, preds)
  } else {
    r <- get_model_ratio_monthly(dates)
    prophet_r <- r * 0.9
    lightgbm_r <- (1 - r) * 0.9

    preds <- predict_with_cache_monthly(stock, dates)

    hostess$set(unname(prophet_r * 100))

    residuals <- predict_residuals_monthly(
      stock, dates, hostess,
      lightgbm_r
    )

    preds$yhat <- preds$yhat + residuals
    preds
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
plot_predictions <- function(predicted) {
  if (is.null(predicted)) {
    return(NULL)
  }
  ggplot2::ggplot(na.omit(predicted), ggplot2::aes(
    x = ref_date, y = yhat,
    ymin = yhat_lower,
    ymax = yhat_upper
  )) +
    ggplot2::geom_ribbon(alpha = 0.5) +
    ggplot2::geom_line(ggplot2::aes(y = yhat_lower), colour = "grey") +
    ggplot2::geom_line(ggplot2::aes(y = yhat_upper), colour = "grey") +
    ggplot2::geom_line(colour = "red") +
    ggplot2::ylab("Predicted price") +
    ggthemes::theme_clean()
}

get_model_ratio_daily <- function(dates) {
  prophet_length <- ceiling(length(dates) / 7)

  lightgbm_length <- lubridate::interval(
    max(daily_training_data$ref_date),
    max(dates)
  ) %/% lubridate::days(7) + 1

  times <- model_times$daily

  prophet_time <- times[1] * prophet_length + times[2]
  lightgbm_time <- times[3] * lightgbm_length + times[4]

  prophet_time / (prophet_time + lightgbm_time)
}

get_model_ratio_monthly <- function(dates) {
  prophet_length <- ceiling(length(dates) / 7)

  lightgbm_length <- lubridate::interval(
    max(monthly_training_data$ref_date),
    max(dates)
  ) %/% months(6) + 1

  times <- model_times$monthly

  prophet_time <- times[1] * prophet_length + times[2]
  lightgbm_time <- times[3] * lightgbm_length + times[4]

  prophet_time / (prophet_time + lightgbm_time)
}

dummy_hostess <- function() {
  list(
    set = function(x) NULL,
    inc = function(x) NULL
  )
}
