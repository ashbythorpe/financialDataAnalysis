#' Model the residuals of stock price predictions.
#'
#' Get the daily or monthly LightGBM models, which are trained to predict the
#' residuals of the Prophet model.
#'
#' @param x Which model to get: either "daily" or "monthly".
#'
#' @details
#' This model is designed to perform iterative forecasting. This means that it
#' can predict residuals for a certain horizon, then assumes these predictions
#' as truth and uses lagged values as predictors, and makes new predictions,
#' and so on.
#'
#' The process to get the models is complex because [readRDS()] does not work
#' for LightGBM models.
#'
#' @returns A fitted [workflows::workflow()] object.
#'
#' @seealso [prophet_models] [predict_price()]
#'
#' @examples
#' get_lightgbm_model("daily")
#'
#' @export
get_lightgbm_model <- function(x) {
  if (x == "daily") {
    wf <- readRDS(system.file(
      "extdata/daily_lightgbm_model.rds",
      package = "financialDataAnalysis"
    ))

    model <- lightgbm::readRDS.lgb.Booster(system.file(
      "extdata/daily_lightgbm_inner_model.rds",
      package = "financialDataAnalysis"
    ))

    wf$fit$fit$fit <- model

    wf
  } else {
    wf <- readRDS(system.file(
      "extdata/monthly_lightgbm_model.rds",
      package = "financialDataAnalysis"
    ))

    model <- lightgbm::readRDS.lgb.Booster(system.file(
      "extdata/monthly_lightgbm_inner_model.rds",
      package = "financialDataAnalysis"
    ))

    wf$fit$fit$fit <- model

    wf
  }
}
