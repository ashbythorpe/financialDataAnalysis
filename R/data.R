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
