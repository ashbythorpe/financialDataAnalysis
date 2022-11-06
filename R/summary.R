#' @export
col_summary <- function(x){
  # This list is defined inside the function so that they run the most updated
  # version of the functions, since the functions are updated every time the
  # function is run, rather than when it is defined (since the list is defined
  # every time the function is run).
  summary_funs <-
    list(
      "Mean" = mean, # "Mean" is the name, mean is the function
      "Median" = median,
      "Minimum" = min,
      "Maximum" = max,
      "Standard deviation" = sd
    )
  # imap means that the names of summary_funs are passed into col_summary_val()
  purrr::imap_chr(summary_funs, .f = col_summary_val, x = x) %>%
    glue::glue_collapse(sep = ", ")
}

col_summary_val <- function(x, fun, fun_name){
  glue::glue("{fun_name}: {round(fun(x, na.rm = T), 2)}")
}

#' @export
score_summary <- function(x, score_type = NULL, lb = NULL, ub = NULL, centre = NULL,
                          inverse = NULL, exponential = NULL, logarithmic = NULL,
                          magnitude = NULL, custom_args = NULL, ...){
  score_spec <- validate_score(
    score_type = score_type, colname = "a", score_name = "",
    weight = 0, lb = lb, ub = ub, centre = centre,
    inverse = inverse, exponential = exponential,
    logarithmic = logarithmic, magnitude = magnitude,
    custom_args = custom_args
  )
  
  if(is.null(score_spec)){
    return(NULL)
  }
  tibble::tibble(column_value = x, score = rlang::inject(score_column(x, !!!score_spec))) %>%
    ggplot2::ggplot(ggplot2::aes(x = column_value, y = score)) +
    ggplot2::geom_line()
}

stock_summary <- function(df, x){
  df[x,]
}
