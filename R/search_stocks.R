#' Search through stocks
#'
#' Filter through a data frame of stocks using a search pattern. The pattern is
#' split up by spaces, and each resulting pattern is matched to the values of
#' the stock name and ticker.
#'
#' @param x The data frame of stocks (e.g. the [default_stock_data]).
#' @param pattern The pattern to use to filter `x`.
#'
#' @returns A filtered version of `x`
#'
#' @examples
#' search_stocks(default_stock_data, "Al p")
#'
#' @export
search_stocks <- function(x, pattern) {
  if (is.null(pattern) || pattern == "") {
    return(x)
  }
  
  final_data <-
    stringr::str_split_1(pattern, " ") %>%
    purrr::map(search_pattern, x = x) %>%
    purrr::reduce(`&`) %>%
    {
      x[., ]
    }
  
  if (nrow(final_data) == 0) {
    return(NULL)
  }
  final_data
}

search_pattern <- function(x, pattern) {
  if (pattern == "") {
    return(TRUE)
  }
  stringr::str_detect(
    paste0("\\Q", stringr::str_to_lower(x$symbol), "\\E"),
    stringr::str_to_lower(pattern)
  ) |
    stringr::str_detect(
      paste0("\\Q", stringr::str_to_lower(x$company_name), "\\E"),
      stringr::str_to_lower(pattern)
    )
}

search_results <- function(x) {
  if (is.null(x)) {
    return(x)
  }
  dplyr::arrange(x, dplyr::desc(favourite)) %>%
    dplyr::select(symbol, company_name)
}
