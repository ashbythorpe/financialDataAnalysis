forecast_stock_daily <- function(object, stock, horizon, training_data,
                                 hostess, r) {
  if (horizon <= 0) {
    return(NULL)
  }

  iterations <- horizon %/% 7 + (horizon %% 7 != 0)
  remove_from_end <- (7 - horizon %% 7) * (horizon %% 7 != 0)

  inc <- r / iterations

  data <- training_data %>%
    dplyr::filter(ticker == stock) %>%
    dplyr::arrange(ds)

  purrr::reduce(
    seq_len(iterations),
    .init = data,
    daily_iteration_wrapper, object = object,
    hostess = hostess, inc = inc
  ) %>%
    dplyr::slice(-1:-nrow(data)) %>%
    dplyr::slice_head(n = -remove_from_end)
}

daily_iteration_wrapper <- function(..., hostess, inc) {
  hostess$inc(unname(inc * 100))

  predict_iteration_daily(...)
}

predict_iteration_daily <- function(data, n, object) {
  ticker <- data$ticker[1]
  max <- max(data$ds)

  future_data <- data %>%
    dplyr::add_row(
      ticker = ticker,
      ds = seq(from = max + 1, to = max + 7, by = "day")
    )

  pred_data <- create_price_features_daily(future_data) %>%
    dplyr::slice_tail(n = 7)

  pred <- stats::predict(object, pred_data)

  dplyr::add_row(
    data,
    ticker = ticker,
    ds = seq(from = max + 1, to = max + 7, by = "day"),
    residuals = pred$.pred
  )
}

forecast_stock_monthly <- function(object, stock, horizon, training_data,
                                   hostess, r) {
  if (horizon <= 0) {
    return(NULL)
  }

  iterations <- horizon %/% 6 + (horizon %% 6 != 0)
  remove_from_end <- (6 - horizon %% 6) * (horizon %% 6 != 0)

  inc <- r / iterations

  data <- training_data %>%
    dplyr::filter(ticker == stock) %>%
    dplyr::arrange(ds)

  purrr::reduce(
    seq_len(iterations),
    .init = data,
    monthly_iteration_wrapper, object = object,
    hostess = hostess, inc = inc
  ) %>%
    dplyr::slice(-1:-nrow(data)) %>%
    dplyr::slice_head(n = -remove_from_end)
}

monthly_iteration_wrapper <- function(..., hostess, inc) {
  hostess$inc(unname(inc * 100))

  predict_iteration_monthly(...)
}

predict_iteration_monthly <- function(data, n, object) {
  ticker <- data$ticker[1]
  max <- max(data$ds)

  future_data <- data %>%
    dplyr::add_row(
      ticker = ticker,
      ds = seq(
        from = lubridate::add_with_rollback(max, months(1)),
        to = lubridate::add_with_rollback(max, months(6)),
        by = "month"
      )
    )

  pred_data <- create_price_features_monthly(future_data) %>%
    dplyr::slice_tail(n = 6)

  pred <- stats::predict(object, pred_data)

  dplyr::add_row(
    data,
    ticker = ticker,
    ds = seq(
      from = lubridate::add_with_rollback(max, months(1)),
      to = lubridate::add_with_rollback(max, months(6)),
      by = "month"
    ),
    residuals = pred$.pred
  )
}

create_price_features_daily <- function(x) {
  x %>%
    dplyr::mutate(
      purrr::map(8:90, dplyr::lag, x = residuals) %>%
        rlang::set_names(paste0("residuals_lag", 8:90)) %>%
        tibble::as_tibble()
    )
}

create_price_features_monthly <- function(x) {
  x %>%
    dplyr::mutate(
      purrr::map(7:60, dplyr::lag, x = residuals) %>%
        rlang::set_names(paste0("residuals_lag", 7:60)) %>%
        tibble::as_tibble()
    )
}
