predict_with_cache_daily <- function(stock, dates) {
  key <- paste0(stringr::str_to_lower(stock), "daily")
  cached_values <- cache$get(key)

  prophet_model <- dplyr::filter(daily_prophet_model, ticker == stock)$fit[[1]]

  if (cachem::is.key_missing(cached_values)) {
    pred_data <- tibble::tibble(ds = dates)
    preds <- stats::predict(prophet_model, pred_data) %>%
      dplyr::select(ds, yhat, yhat_lower, yhat_upper)

    cache$set(key, preds)
  } else if (!any(cached_values$ds %in% dates)) {
    pred_data <- tibble::tibble(ds = dates)
    preds <- stats::predict(prophet_model, pred_data) %>%
      dplyr::select(ds, yhat, yhat_lower, yhat_upper)

    cache$set(
      key,
      dplyr::bind_rows(cached_values, preds)
    )
  } else if (!all(cached_values$ds %in% dates)) {
    known_dates <- dates[dates %in% cached_values$ds]
    new_dates <- dates[!dates %in% cached_values$ds]
    known_data <- cached_values[cached_values$ds %in% known_dates]

    pred_data <- tibble::tibble(ds = known_dates)
    new_data <- stats::predict(prophet_model, pred_data) %>%
      dplyr::select(ds, yhat, yhat_lower, yhat_upper)

    preds <- dplyr::bind_rows(known_data, new_data)

    cache$set(
      key,
      dplyr::bind_rows(cached_values, new_data)
    )
  } else {
    preds <- cached_values[cached_values$ds %in% dates]
  }

  preds %>%
    dplyr::select(-ds)
}

predict_with_cache_monthly <- function(stock, dates) {
  key <- paste0(stringr::str_to_lower(stock), "monthly")
  cached_values <- cache$get(key)

  prophet_model <- dplyr::filter(monthly_prophet_model, ticker == stock)$fit[[1]]

  if (cachem::is.key_missing(cached_values)) {
    pred_data <- tibble::tibble(ds = dates)
    preds <- stats::predict(prophet_model, pred_data) %>%
      dplyr::select(ds, yhat, yhat_lower, yhat_upper)

    cache$set(key, preds)
  } else if (!any(cached_values$ds %in% dates)) {
    pred_data <- tibble::tibble(ds = dates)
    preds <- stats::predict(prophet_model, pred_data) %>%
      dplyr::select(ds, yhat, yhat_lower, yhat_upper)

    cache$set(
      key,
      dplyr::bind_rows(cached_values, preds)
    )
  } else if (!all(cached_values$ds %in% dates)) {
    known_dates <- dates[dates %in% cached_values$ds]
    new_dates <- dates[!dates %in% cached_values$ds]
    known_data <- cached_values[cached_values$ds %in% known_dates]

    pred_data <- tibble::tibble(ds = known_dates)
    new_data <- stats::predict(prophet_model, pred_data) %>%
      dplyr::select(ds, yhat, yhat_lower, yhat_upper)

    preds <- dplyr::bind_rows(known_data, new_data)

    cache$set(
      key,
      dplyr::bind_rows(cached_values, new_data)
    )
  } else {
    preds <- cached_values[cached_values$ds %in% dates]
  }

  preds %>%
    dplyr::select(-ds)
}
