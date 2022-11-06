## code to prepare `daily_lightgbm_model` dataset goes here

library(bonsai)

clean_date <- function(x) {
  x %>%
    timetk::pad_by_time(ds, .by = "day") %>%
    dplyr::mutate(residuals = timetk::ts_impute_vec(residuals))
}

pred <- readRDS("daily_fit.rds")

data <- pred %>%
  dplyr::select(-fit) %>%
  tidyr::unnest(c(data, pred), names_repair = "unique") %>%
  dplyr::select(-ds...4) %>%
  dplyr::rename(ds = "ds...2") %>%
  dplyr::mutate(residuals = y - yhat) %>%
  dplyr::select(ticker, ds, residuals) %>%
  dplyr::arrange(ds)

data %>%
  dplyr::filter(ticker == "GOOGL") %>%
  ggplot2::ggplot(ggplot2::aes(x = ds, y = residuals, colour = ticker)) +
  ggplot2::geom_line()

initial_split <- rsample::initial_time_split(data, prop = 0.8)

data_tr <- rsample::training(initial_split) %>%
  dplyr::group_by(ticker) %>%
  clean_date() %>%
  create_price_features_daily() %>%
  dplyr::ungroup()

data_tst <- rsample::testing(initial_split)

splits <- timetk::time_series_cv(
  data_tr,
  date_var = ds,
  initial = "1 month",
  assess = "10 days",
  cumulative = TRUE,
  skip = "15 days"
)

lightgbm_recipe <- recipes::recipe(data_tr, residuals ~ .) %>%
  recipes::step_date(ds, features = c(
    "month", "dow", "quarter", "week", "decimal"
  )) %>%
  recipes::step_harmonic(ds, frequency = 1:5, cycle_size = 30,
                         keep_original_cols = TRUE) %>%
  recipes::step_rm(ds)

lightgbm_recipe %>%
  recipes::prep() %>%
  recipes::bake(NULL)

lightgbm_model <- boost_tree(
  mode = "regression",
  learn_rate = tune(),
  tree_depth = tune(),
  min_n = tune(),
  trees = 1000
) %>%
  set_engine("lightgbm")

lightgbm_wf <- workflows::workflow() %>%
  workflows::add_model(lightgbm_model) %>%
  workflows::add_recipe(lightgbm_recipe)

tune_res <- tune::tune_grid(
  lightgbm_wf,
  resamples = splits,
  control = tune::control_grid(verbose = TRUE)
)

saveRDS(tune_res, "tune_res.rds")
tune_res <- readRDS("tune_res.rds")

bayes_res <- tune::tune_bayes(
  lightgbm_wf,
  resamples = splits,
  initial = tune_res,
  metrics = yardstick::metric_set(yardstick::rsq, yardstick::rmse),
  control = tune::control_bayes(
    verbose = TRUE, verbose_iter = TRUE
  )
)

tune::show_best(bayes_res, "rmse")
tune::show_best(bayes_res, "rsq")

best <- tune::select_best(bayes_res, "rsq")

best <- list(
  min_n = 7,
  tree_depth = 12,
  learn_rate = 0.0398679046932473
)

final_wf <- tune::finalize_workflow(lightgbm_wf, best)

fit <- fit(final_wf, data_tr)

pred <- forecast_stock_daily(fit, "GOOGL", 36,
                             rsample::training(initial_split) %>%
                               dplyr::group_by(ticker) %>%
                               clean_date() %>%
                               dplyr::ungroup())

pred %>%
  ggplot2::ggplot(ggplot2::aes(x = ds, y = residuals)) +
  ggplot2::geom_line()

data_tr %>%
  dplyr::filter(ticker == "GOOGL") %>%
  dplyr::bind_rows(pred) %>%
  ggplot2::ggplot(ggplot2::aes(x = ds, y = residuals)) +
  ggplot2::geom_line()

compared <- dplyr::left_join(data_tst, pred %>%
                               dplyr::rename(.pred = "residuals"))

compared %>%
  dplyr::filter(ticker == "GOOGL") %>%
  tidyr::pivot_longer(cols = c(residuals, .pred)) %>%
  ggplot2::ggplot(ggplot2::aes(x = ds, y = value, colour = name)) +
  ggplot2::geom_line()

yardstick::metrics(compared, residuals, .pred)

data_with_features <- data %>%
  dplyr::group_by(ticker) %>%
  clean_date() %>%
  create_price_features_daily() %>%
  dplyr::ungroup()

final_fit <- fit(final_wf, data_with_features) %>%
  butcher::butcher() # Drastically reduce the size of the workflow

final_fit <- daily_lightgbm_model %>%
  butcher::butcher()

lightgbm::saveRDS.lgb.Booster(final_fit$fit$fit$fit, 
                              "data/daily_lightgbm_inner_model.rds")

final_fit$fit$fit$fit <- NULL

saveRDS(final_fit, "data/daily_lightgbm_model.rds")
