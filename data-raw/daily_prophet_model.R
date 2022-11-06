## code to prepare `daily_prophet_model` dataset goes here

data <- daily_stock_data %>%
  dplyr::select(ticker, ref_date, price_adjusted) %>%
  dplyr::arrange(ref_date) %>%
  dplyr::mutate(ticker = forcats::as_factor(ticker)) %>%
  dplyr::rename(ds = "ref_date", y = "price_adjusted")

fit <- data %>%
  tidyr::nest(data = -ticker) %>%
  dplyr::mutate(fit = purrr::map(data, prophet::prophet, 
                                 daily.seasonality = TRUE,
                                 seasonality.mode = "multiplicative"))

pred <- fit %>%
  dplyr::mutate(
    pred = purrr::pmap(
      list(fit, data, cli::cli_progress_along(fit)), ~ {predict(.x, .y)}
    )
  )

daily_prophet_model <- fit

usethis::use_data(daily_prophet_model, overwrite = TRUE)
