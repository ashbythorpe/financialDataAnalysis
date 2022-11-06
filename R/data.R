#' 
"scores_init"

#' Predict the price of a stock over time
#' 
#' Use a Prophet model to predict the price of a stock over time.
#' 
#' 
#' @rdname prophet_model
"daily_prophet_model"



#' @include get_lightgbm_model.R
#' 
#' @export
daily_lightgbm_model <- get_lightgbm_model("daily")

#'
#' @export
monthly_lightgbm_model <- get_lightgbm_model("monthly")
