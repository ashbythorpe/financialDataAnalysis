library(tidyverse)
library(tidymodels)
library(modeltime)
library(boostime)
library(timetk)
library(finetune)
library(doFuture)
library(fst)
library(workflowsets)
library(stacks)

registerDoFuture()
plan("default")

monthly_data <- read_fst("monthly_price_data.fst") %>%
  as_tibble()
daily_data <- read_fst("daily_price_data.fst") %>%
  as_tibble()

full_data <- full_join(monthly_data, daily_data) %>%
  select(ticker, ref_date, price_adjusted) %>%
  arrange(ref_date) %>%
  rename(date = ref_date, price = price_adjusted)

ggplot(full_data, aes(x = data, y = price, color = ticker)) +
  geom_line() +
  scale_color_viridis_d()

initial_split <- initial_time_split(full_data, prop = 0.8)

data_tr <- training(initial_split)


splits <- sliding_index(data_tr, "date", lookback = Inf, skip = 12*50, complete = F,
                        step = 12*50, assess_stop = 6*50)

prophet_model <-
  prophet_reg(changepoint_num = tune(),
              changepoint_range = tune(),
              prior_scale_changepoints = tune(),
              prior_scale_holidays = tune(),
              prior_scale_seasonality = tune()) %>%
  set_engine("prophet")

prophet_recipe <-
  recipe(data_tr, price_adjusted ~ .)

prophet_wf <-
  workflow() %>%
  add_model(prophet_model) %>%
  add_recipe(prophet_recipe)

prophet_xgboost_model <-
  prophet_boost(changepoint_num = tune(),
                changepoint_range = tune(),
                prior_scale_changepoints = tune(),
                prior_scale_holidays = tune(),
                prior_scale_seasonality = tune(),
                trees = 1000,
                tree_depth = tune(),
                min_n = tune(),
                mtry = tune()) %>%
  set_engine("prophet_xgboost")

non_prophet_recipe <-
  recipe(data_tr, price_adjusted ~ .) %>%
  step_timeseries_signature(ref_date)

prophet_xgboost_wf <-
  workflow() %>%
  add_model(prophet_xgboost_model) %>%
  add_recipe(non_prophet_recipe)

prophet_xgboost_params <-
  prophet_xgboost_wf %>%
  extract_parameter_set_dials() %>%
  finalize(data_tr)

xgboost_model <-
  boost_tree(trees = 1000,
             tree_depth = tune(), min_n = tune(), mtry = tune())

xgboost_wf <-
  workflow() %>%
  add_model(xgboost_model) %>%
  add_recipe(non_prophet_recipe)

xgboost_params <-
  xgboost_wf %>%
  extract_parameter_set_dials() %>%
  finalize(data_tr)

wfs <-
  as_workflow_set(prophet = prophet_wf,
                  prophet_xgboost = prophet_xgboost_wf,
                  xgboost = xgboost_wf) %>%
  option_add(param_info = prophet_xgboost_params, id = "prophet_xgboost") %>%
  option_add(param_info = xgboost_params, id = "xgboost") %>%
  option_add(control = control_race(save_pred = T, save_workflow = T))

tuned <-
  workflow_map(wfs, "tune_race_anova")

stacks <- stacks() %>%
  add_candidates(tuned)

fitted_stack <-
  blend_predictions(stacks) %>%
  fit_members()

fitted_stack %>%
  predict(testing(initial_split))
