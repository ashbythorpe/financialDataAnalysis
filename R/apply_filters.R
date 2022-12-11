#' Apply a table of filters to a data frame.
#'
#' Use a data frame, where each row represents a filter, to filter another data
#' frame using the value of its columns.
#'
#' @param df The data frame to filter. See [filters_init].
#' @param filters A data frame of filters.
#'
#' @returns
#' A filtered version of `df` (a subset of the rows), or `NULL` if all rows are
#' removed.
#'
#' @seealso
#' * [filters_init] for the filters table.
#' * [add_filter()], [edit_filter()] and [remove_filter()] to manipulate the
#'   filters table.
#'
#' @examples
#' data <- tibble::tibble(
#'   x = 1:10,
#'   y = "a"
#' )
#'
#' filters <- add_filter(filters_init, "x", data)
#' filters <- edit_filter(filters, 1, min = 1, max = 5)
#' apply_filters(data, filters)
#'
#' @export
apply_filters <- function(df, filters) {
  if (nrow(filters) == 0) {
    return(df)
  }
  filtered <- purrr::pmap(filters, ~ filter_column(df[[..2]], ...)) %>%
    purrr::reduce(`&`, .init = T) %>%
    dplyr::filter(.data = df)
  if (nrow(filtered) == 0) {
    return(NULL)
  }
  filtered
}

filter_column <- function(x, type, pattern, min, max, ...) {
  if (type == "character") {
    # The pattern must not be treated as a regular expression
    stringr::str_detect(x, paste0("\\Q", pattern, "\\E"))
  } else {
    x >= min & x <= max
  }
}
