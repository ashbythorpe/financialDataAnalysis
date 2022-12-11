#' A large table of stock data
#'
#' A large amount of data fetched from 'iex.cloud' about almost 500 stocks.
#' Each row represents a stock.
#'
#' @format
#' A tibble with 497 rows and 100 variables.
#'
#' @examples
#' default_stock_data
#'
"default_stock_data"

#' Stock price training data
#'
#' A set of tables containing the stock price and the residuals of the Prophet
#' model over a daily or monthly period. This data was used to train the
#' LightGBM model.
#'
#' @format
#' A tibble with 63,617 / 57,793 rows and 4 variables.
#' \describe{
#'   \item{ticker}{The ticker/symbol of the stock}
#'   \item{ref_date}{The date on which the price was recorded}
#'   \item{price}{The price of the stock (USD)}
#'   \item{residuals}{The residuals (difference between predictions and actual
#'   price) of the Prophet model}
#' }
#'
#' @seealso [prophet_models] [get_lightgbm_model()]
#'
#' @examples
#' daily_training_data
#' monthly_training_data
#'
#' @name training_data
"daily_training_data"

#' @name training_data
"monthly_training_data"

#' A table of score specifications
#'
#' The initial state of a table where each row represents a specification of a
#' score.
#'
#' @format
#' A tibble with 0 rows and 12 variables.
#' \describe{
#'   \item{score_type}{The method to use when creating the score: either
#'   "Linear", "Peak" or "Custom coordinates"}
#'   \item{colname}{The name of the column to score}
#'   \item{score_name}{The resulting name of the score}
#'   \item{weight}{The weight of the score when calculating the final score}
#'   \item{lb}{The lower bound of the score}
#'   \item{ub}{The upper bound of the score}
#'   \item{centre}{The centre of the score}
#'   \item{inverse}{Whether to invert a peak score}
#'   \item{exponential}{Whether to apply an exponential transformation to the
#'   score}
#'   \item{logarithmic}{Whether to invert the exponential transformation}
#'   \item{magnitude}{The magnitude of the exponential transformation}
#'   \item{custom_args}{A data frame of coordinates used to create a custom
#'   score}
#' }
#'
#' @seealso
#' * [create_score()] and [delete_scores()] to manipulate the table.
#' * [apply_scores()] for the creation of the actual scores.
#'
#' @examples
#' scores_init
#'
"scores_init"

#' A table of filters
#'
#' The initial state of a data frame, where each row represents a filter.
#'
#' @format
#' A tibble with 0 rows and 5 variables
#' \describe{
#'   \item{type}{The filter type, either "character" or "numeric"}
#'   \item{colname}{The name of the column to filter}
#'   \item{pattern}{For character filters, the pattern to match to the column
#'   value}
#'   \item{min}{For numeric filters, the minimum value of the column}
#'   \item{max}{For numeric filters, the maximum value of the column}
#' }
#'
#' @details
#' Character filters work on columns that contain strings (sequences of
#' letters). For each row, if the value of the column contains the `pattern`
#' of the filter, it is kept in the filtered data frame. Otherwise, the row is
#' filtered out.
#'
#' For example, if the pattern was "at", and the column values were "cat",
#' "match" and "bit", then the first two values would be kept in the filtered
#' data frame since they contain the pattern.
#'
#' Numeric filters work on columns containing numbers. For each row, if the
#' column value is between the `min` value and the `max` value, it is kept in
#' the filtered data frame. Otherwise, it is removed.
#'
#' @seealso
#' * [add_filter()], [edit_filter()] and [remove_filter()] to manipulate the
#'   filters table.
#' * [apply_filters()] to apply the filters table you have created to some data.
#'
#' @examples
#' filters_init
#'
"filters_init"

#' Predict the price of a stock over time
#'
#' Use a Prophet model to predict the price of a stock over time. Use
#' `daily_prophet_model` for daily predictions, and `monthly_prophet_model` for
#' monthly predictions.
#'
#' @format
#' A tibble with 491 / 470 rows and 3 variables.
#' \describe{
#'   \item{ticker}{The ticker that the model was fitted on}
#'   \item{data}{The training data that the model was fitted on}
#'   \item{fit}{The fitted Prophet model object}
#' }
#'
#' @details
#' Each model was fitted on the time series data for an individual stock.
#'
#' @seealso [predict_price()] [get_lightgbm_model()]
#'
#' @examples
#' daily_prophet_model
#' monthly_prophet_model
#'
#' @name prophet_models
"daily_prophet_model"

#' @name prophet_models
"monthly_prophet_model"
