library(tidyverse)
library(tidymodels)
library(modeltime)
library(bonsai)
library(timetk)
library(lightgbm)
library(finetune)
library(workflowsets)
library(stacks)
library(timetk)

monthly_data <- readRDS("monthly_data.rds")

data <- monthly_data %>%
  dplyr::select(ticker, ref_date, price_adjusted) %>%
  dplyr::arrange(ref_date)

ggplot(data, aes(x = ref_date, y = price_adjusted, colour = ticker)) +
  geom_line() +
  scale_colour_viridis_d() +
  theme(legend.position = "none")

initial_split <- rsample::initial_time_split(data, prop = 0.8)

data_tr <- rsample::training(initial_split)
data_tst <- rsample::testing(initial_split)

splits <- timetk::time_series_cv(
  data_tr,
  date_var = ref_date,
  initial = "1 year",
  assess = "6 months",
  cumulative = TRUE,
  skip = "6 months",
  slice_limit = 7
)

base_recipe <- recipes::recipe(data_tr, price_adjusted ~ ref_date + ticker)

prophet_recipe <- base_recipe %>%
  embed::step_lencode_mixed(ticker, outcome = "price_adjusted")

prophet_model <- 
  prophet_reg(
    changepoint_num = tune(),
    changepoint_range = tune(),
    prior_scale_changepoints = tune(),
    prior_scale_seasonality = tune(),
    prior_scale_holidays = tune()
  ) %>%
  set_engine("prophet")

prophet_wf <- workflow() %>%
  add_model(prophet_model) %>%
  add_recipe(prophet_recipe)

boost_recipe <- base_recipe %>%
  timetk::step_timeseries_signature(ref_date) %>%
  recipes::step_rm(matches("(.iso$)|(.xts$)|(day)|(hour)|(minute)|(second)|(am.pm)")) %>%
  recipes::step_mutate(ref_date_month = factor(ref_date_month, ordered = TRUE))

lightgbm_model <- boost_tree(
  mode = "regression",
  learn_rate = tune(),
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_engine("lightgbm")

lightgbm_wf <- workflows::workflow() %>%
  workflows::add_model(lightgbm_model) %>%
  workflows::add_recipe(boost_recipe)

wfset <- as_workflow_set(
  prophet = prophet_wf,
  lightgbm = lightgbm_wf
)

tuned_wfset <- 
  workflow_map(
    wfset,
    "tune_race_anova",
    resamples = splits,
    control = finetune::control_race(verbose = T,
                                     allow_par = F,
                                     save_pred = T,
                                     save_workflow = T)
  )

rank_results(tuned_wfset)

rank_results(tuned_wfset, select_best = T)

stack <- stacks() %>%
  add_candidates(tuned_wfset)

blended <- blend_predictions(stack, penalty = 10^seq(-2, -0.5, length = 20))

blended

autoplot(blended)

fitted <- fit_members(blended)

predictions <- predict(fitted, data_tst)

rmse_vec(predictions$.pred, data_tst$price_adjusted)

data_tst %>%
  bind_cols(predictions) %>%
  pivot_longer(c(price_adjusted, .pred)) %>%
  filter(ticker == "GOOG") %>%
  ggplot(aes(x = ref_date, y = value, colour = name)) +
  geom_line()

blended$train <- data

monthly_stock_model <- fit_members(blended)

saveRDS(fitted, "monthly_model.rds")
