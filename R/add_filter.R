#' Add a filter to a table of filters
#'
#' Create a filter for a data frame, and add it to a table of filters, where
#' each filter is represented by a row. The filter is only added if the column
#' you specify to filter is valid.
#'
#' @param filters A data frame of filters. See [filters_init].
#' @param colname A string specifying the column to filter.
#' @param data The data that is to be filtered.
#'
#' @details
#' The function will either create a character filter or a numeric filter,
#' depending on the type of the column. See [filters_init] for how these two
#' filters work.
#'
#' @returns `filters` with an added row, provided that `colname` is valid.
#'
#' @seealso
#' * [filters_init] for the filters table.
#' * [edit_filter()] and [remove_filter()] to further edit the filters table.
#' * [apply_filters()] to apply the filters you have created to your data.
#'
#' @examples
#' data <- tibble::tibble(
#'   x = 1:10,
#'   y = "a"
#' )
#'
#' add_filter(filters_init, "x", data)
#' add_filter(filters_init, "y", data)
#' add_filter(filters_init, "z", data)
#'
#' @export
add_filter <- function(filters, colname, data) {
  if (is.null(colname) || !colname %in% colnames(data)) {
    return(filters)
  }
  column <- data[[colname]]
  if (is.character(column)) {
    # Add a character filter
    filter <- tibble::tibble_row(type = "character", colname = colname, pattern = "")
  } else {
    # Add a numeric filter
    filter <- tibble::tibble_row(
      type = "numeric", colname = colname,
      min = min(column, na.rm = T),
      max = max(column, na.rm = T)
    )
  }
  tibble::add_row(filters, filter)
}

#' Edit a filter in a table of filters
#'
#' Edit a filter for a data frame, by editing a row of a data frame, where each
#' row represents a filter. If your edit results in an invalid filter, the edit
#' does not succeed.
#'
#' @param filters A data frame of filters. See [filters_init].
#' @param x A number specifying the filter number to edit (indexed at 1).
#' @param pattern A string.
#' @param min A number.
#' @param max A number.
#'
#' @returns
#' `filters` with an edited row, provided that the necessary arguments are
#' valid.
#'
#' @seealso
#' * [filters_init] for the filters table.
#' * [add_filter()] and [remove_filter()] to further edit the filters table.
#' * [apply_filters()] to apply the filters you have created to your data.
#'
#' @examples
#' data <- tibble::tibble(
#'   x = 1:10,
#'   y = "a"
#' )
#'
#' filters <- add_filter(filters_init, "x", data)
#' edit_filter(filters, 1, min = 1, max = 5)
#'
#' @export
edit_filter <- function(filters, x, pattern = NULL, min = NULL, max = NULL) {
  if (x > nrow(filters)) {
    return(filters)
  }
  
  # Get the type of the current filter
  type <- filters[x, ]$type
  if (type == "character") {
    edit_character_filter(filters, x, pattern)
  } else {
    edit_numeric_filter(filters, x, min, max)
  }
}

edit_character_filter <- function(filters, x, pattern) {
  if (is.null(pattern)) {
    return(filters)
  }
  filters[x, "pattern"] <- pattern
  filters
}

edit_numeric_filter <- function(filters, x, min, max) {
  if (is.null(min) || is.null(max)) {
    return(filters)
  }
  filters[x, "min"] <- min
  filters[x, "max"] <- max
  filters
}

#' Remove a filter from a table of filters
#'
#' Delete a filter for a data frame, by removing a row from a data frame, where
#' each row represents a filter.
#'
#' @param filters A data frame of filters. See [filters_init].
#' @param x A number specifying the filter number to remove (indexed at 1).
#'
#' @returns
#' `filters` with one less row.
#'
#' @seealso
#' * [filters_init] for the filters table.
#' * [add_filter()] and [edit_filter()] to further edit the filters table.
#' * [apply_filters()] to apply the filters you have created to your data.
#'
#' @examples
#' data <- tibble::tibble(
#'   x = 1:10,
#'   y = "a"
#' )
#'
#' filters <- add_filter(filters_init, "x", data)
#' remove_filter(filters, 1)
#'
#' @export
remove_filter <- function(filters, x) {
  if (x > nrow(filters)) {
    return(filters)
  }
  dplyr::slice(filters, -x)
}
