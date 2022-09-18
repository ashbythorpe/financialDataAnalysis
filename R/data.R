#' ```
#' method_fun <- 
#'   list(
#'     line = "geom_line",
#'     scatter = "geom_point",
#'     histogram = "geom_histogram"
#'   )
#' ```
#' 
"method_fun"

#' ```
#' scores_init <-
#'   tibble::tibble(
#'     score_type = character(),
#'     colname = character(),
#'     score_name = character(),
#'     weight = numeric(),
#'     lb = numeric(),
#'     ub = numeric(),
#'     centre = numeric(),
#'     inverse = logical(),
#'     exponential = logical(),
#'     logarithmic = logical(),
#'     magnitude = numeric(),
#'     custom_args = list(),
#'     .rows = 0
#'   )
#' ```
"scores_init"
