#' Mark a stock as a 'favourite'
#'
#' Switch the 'favourite' column for a specified row. This feature is no longer
#' included in the final application.
#'
#' @param df The data frame
#' @param x The row to 'favourite'
#'
#' @returns A modified version of `df`.
#'
#' @examples
#' data <- tibble::tibble(
#'   x = 1:10,
#'   favourite = FALSE
#' )
#'
#' favourite_stock(data, 5)
#'
#' @export
favourite_stock <- function(df, x) {
  if (is.null(df)) {
    return(NULL)
  } else if (is.null(x) || x > nrow(df)) {
    return(df)
  }
  df[x, "favourite"] <- !df[x, ]$favourite
  df
}
