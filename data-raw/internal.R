## code to prepare `internal` dataset goes here

args_type <- list(
  x = 'any',
  y = 'any',
  colour = 'categorical',
  size = 'continuous',
  shape = 'categorical'
)

method_args <- list(
  line = c('x', 'y', 'colour'),
  scatter = c('x', 'y', 'colour', 'size', 'shape'),
  histogram = c('x', 'colour', 'size')
)

method_required_args <- list(
  line = c('x', 'y'),
  scatter = c('x', 'y'),
  histogram = c('x')
)

test_predict_price <- function(x, freq = "daily") {
  if(freq == "daily") {
    n <- 7*x
    dates <- seq(lubridate::today(), lubridate::today() + n, by = "day")
    
    prophet_model <- dplyr::filter(daily_prophet_model, ticker == "GOOGL")$fit[[1]]
    pred_data <- tibble::tibble(ds = dates)
    
    system.time({
      preds <- withr::with_package("prophet", {
        stats::predict(prophet_model, pred_data) %>%
          dplyr::select(ds, yhat) %>%
          dplyr::mutate(ds = lubridate::date(ds))
      })
    })["elapsed"] %>%
      unclass()
  } else {
    n <- 6*x
    dates <- seq(lubridate::today(), 
                 lubridate::add_with_rollback(lubridate::today(), months(n)), 
                 by = "month")
    
    prophet_model <- dplyr::filter(monthly_prophet_model, ticker == "GOOGL")$fit[[1]]
    pred_data <- tibble::tibble(ds = dates)
    
    system.time({
      preds <- withr::with_package("prophet", {
        stats::predict(prophet_model, pred_data) %>%
          dplyr::select(ds, yhat) %>%
          dplyr::mutate(ds = lubridate::date(ds))
      })
    })["elapsed"] %>%
      unclass()
  }
}

test_predict_residuals <- function(x, freq = "daily") {
  if(freq == "daily") {
    n <- 7*x
    system.time({
      forecast_stock_daily(
        get_lightgbm_model("daily"),
        "GOOGL",
        n,
        daily_training_data %>%
          dplyr::rename(ds = "ref_date")
      )
    })["elapsed"] %>%
      unclass()
  } else {
    n <- 6*x
    system.time({
      forecast_stock_monthly(
        get_lightgbm_model("monthly"),
        "GOOGL",
        n,
        monthly_training_data %>%
          dplyr::rename(ds = "ref_date")
      )
    })["elapsed"] %>%
      unclass()
  }
}

times <- purrr::map_dbl(1:50, test_predict_price)
times2 <- purrr::map_dbl(1:50, test_predict_residuals)

model1 <- lm(y ~ x, tibble::tibble(x = 1:50, y = times))
model2 <- lm(y ~ x, tibble::tibble(x = 1:50, y = times2))

coefs <- c(model1$coefficients, model2$coefficients)

ordered_coefs <- coefs[c(2,1,4,3)]

times_monthly <- purrr::map_dbl(1:50, test_predict_price, freq = "monthly")
times2_monthly <- purrr::map_dbl(1:50, test_predict_residuals, freq = "monthly")

model1_monthly <- lm(y ~ x, tibble::tibble(x = 1:50, y = times_monthly))
model2_monthly <- lm(y ~ x, tibble::tibble(x = 1:50, y = times2_monthly))

coefs_monthly <- c(model1_monthly$coefficients, model2_monthly$coefficients)

ordered_coefs_monthly <- coefs_monthly[c(2,1,4,3)]

model_times <- list(
  daily = ordered_coefs,
  monthly = ordered_coefs_monthly
)

usethis::use_data(
  args_type,
  method_args,
  method_required_args,
  model_times,
  internal = TRUE, 
  overwrite = TRUE
)
