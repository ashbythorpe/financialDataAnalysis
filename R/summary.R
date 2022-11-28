#' Generate a summary of a column
#' 
#' Generate a string summary of a numeric column. Shows important information
#' such as the mean and standard deviation.
#' 
#' @param x The column to summarise, in vector form.
#' 
#' @returns A string: the summary of the column.
#' 
#' @examples 
#' col_summary(1:100)
#' 
#' @export
col_summary <- function(x){
  # This list is defined inside the function so that they run the most updated
  # version of the functions, since the functions are updated every time the
  # function is run, rather than when it is defined (since the list is defined
  # every time the function is run).
  summary_funs <-
    list(
      "Mean" = mean, # "Mean" is the name, mean is the function
      "Median" = stats::median,
      "Minimum" = min,
      "Maximum" = max,
      "Standard deviation" = stats::sd
    )
  # imap means that the names of summary_funs are passed into col_summary_val()
  purrr::imap_chr(summary_funs, .f = col_summary_val, x = x) %>%
    glue::glue_collapse(sep = ", ")
}

col_summary_val <- function(x, fun, fun_name){
  glue::glue("{fun_name}: {round(fun(x, na.rm = T), 2)}")
}

#' Generate a summary of a score
#' 
#' Generate a summary of a score currently being created. Generates a line graph
#' comparing the column value and the corresponding score.
#' 
#' @param x The column being scored.
#' @param ... The score arguments. See [create_score()].
#' 
#' @returns A [ggplot2::ggplot()] object containing the graph.
#' 
#' @examples 
#' score_summary(
#'   1:100, score_type = "Linear", lb = 15, ub = 70, exponential = FALSE
#' )
#' 
#' @export
score_summary <- function(x, ...){
  score_spec <- validate_score(
    colname = "a", score_name = "", weight = 0, ...
  )
  
  if(is.null(score_spec)){
    return(NULL)
  }
  tibble::tibble(column_value = x, score = rlang::inject(score_column(x, !!!score_spec))) %>%
    ggplot2::ggplot(ggplot2::aes(x = column_value, y = score)) +
    ggplot2::geom_line() +
    ggplot2::ylim(0,1) +
    ggthemes::theme_clean()
}

#' Create a summary of a stock
#' 
#' Summarise a stock by showing the data corresponding to the stock.
#' 
#' @param df The stock data. See [default_stock_data].
#' @param x The row number of the stock.
#' 
#' @returns A [tibble::tibble_row()].
#' 
#' @examples 
#' stock_summary(default_stock_data, 1)
#' 
#' @export
stock_summary <- function(df, x){
  df[x,]
}
