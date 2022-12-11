## code to prepare `monthly_prophet_model` dataset goes here

data <- monthly_stock_data %>%
  dplyr::select(ticker, ref_date, price_adjusted) %>%
  dplyr::arrange(ref_date) %>%
  dplyr::mutate(ticker = forcats::as_factor(ticker)) %>%
  dplyr::rename(ds = "ref_date", y = "price_adjusted")

fit <- data %>%
  tidyr::nest(data = -ticker) %>%
  dplyr::mutate(
    fit = purrr::map(data, prophet::prophet,
      seasonality.mode = "multiplicative"
    )
  )

monthly_prophet_model <- fit

usethis::use_data(monthly_prophet_model, overwrite = TRUE)

pred <- fit %>%
  dplyr::mutate(
    pred = purrr::pmap(
      list(fit, data, cli::cli_progress_along(fit)), ~ {
        predict(.x, .y)
      }
    )
  )

monthly_training_data <- pred %>%
  dplyr::select(-fit) %>%
  tidyr::unnest(c(data, pred), names_repair = "unique") %>%
  dplyr::select(-ds...4) %>%
  dplyr::rename(ds = "ds...2") %>%
  dplyr::mutate(residuals = y - yhat) %>%
  dplyr::rename(price_adjusted = "y", ref_date = "ds") %>%
  dplyr::select(ticker, ref_date, price_adjusted, residuals)

use_data(monthly_training_data, overwrite = TRUE)
