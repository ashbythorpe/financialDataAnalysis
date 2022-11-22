#' Create a customisable plot
#' 
#' Create a plot which is fully customisable by the user.
#' 
#' @param df The data to plot.
#' @param plotting_method The type of plot to create (either "line", "scatter"
#'   or "histogram")
#' @param ... A set of arguments mapping certain variables to aesthetics. Each
#' argument should be specified in the format `aesthetic = "column_name"`, where
#' `aesthetic` is a visual property that a variable can be mapped to (e.g. x, 
#' colour), and `column_name` is the name of a column in your data.
#' 
#' @details 
#' # Line graphs
#' Create line graphs passing in "line" to the `plotting_method` argument.
#' 
#' Line graphs accept the following aesthetics:
#'   
#' * `x` - the variable on the x axis.
#' * `y` - the variable on the y axis.
#' * `colour` - the colour of the line.
#' 
#' `x` and `y` are required arguments, meaning that they must be supplied for a 
#' plot to be outputted.
#' 
#' # Scatter graphs
#' Create scatter graphs passing in "scatter" to the `plotting_method` argument.
#' 
#' Scatter graphs accept the following aesthetics:
#'   
#' * `x` - the variable on the x axis.
#' * `y` - the variable on the y axis.
#' * `colour` - the colour of the point.
#' * `size` - the size of the point.
#' * `shape` - the shape of the point.
#' 
#' `x` and `y` are required arguments, meaning that they must be supplied for a 
#' plot to be outputted.
#' 
#' # Histograms
#' Create histograms passing in "histogram" to the `plotting_method` argument.
#' 
#' Histograms accept the following aesthetics:
#'   
#' * `x` - the variable on the x axis.
#' * `colour` - the colour of the bar.
#' * `size` - the size of the bar.
#' 
#' `x` is a required arguments, meaning that it must be supplied for a plot to 
#' be outputted.
#' 
#' The y aesthetic of a histogram is the frequency density of the x coordinate.
#' 
#' @returns A [ggplot2::ggplot()] object.
#' 
#' @examples 
#' data <- tibble::tibble(
#'   a = 1:10,
#'   b = 10:1
#' )
#' 
#' custom_plot(data, "line", x = "a", y = "b")
#' 
#' @export
custom_plot <- function(df, plotting_method, ...){
  plot_args <- rlang::list2(...)
  
  method <- validate_plotting_method(plotting_method)
  
  plot_args <- validate_plot_args(plot_args, df) %>%
    subset_plot_args(method)
  
  if(is.null(plot_args) || is.null(method)){
    return(NULL)
  }
  
  # The plot only errors when it is printed: assign the result of this function
  # to a variable and it won't produce an error.
  create_plot(df, plot_args, method)
}

create_plot <- function(df, args, method){
  aesthetics <- args %>%
    rlang::data_syms() %>%
    ggplot2::aes(,,!!!.)
  
  layer <- 
    switch(method,
      line = ggplot2::geom_line(),
      scatter = ggplot2::geom_point(),
      histogram = ggplot2::geom_histogram(
        ggplot2::aes(y = ggplot2::after_stat(density))
      ), # Use frequency density rather than count
      NULL
    )
  
  ggplot2::ggplot(data = df, aesthetics) +
    layer
}

validate_plotting_method <- function(x){
  if(is.null(x) || !x %in% names(method_args)){
    return(NULL)
  }
  x
}

validate_plot_args <- function(args, df){
  valid_args <- purrr::keep(args, ~ {
    is.character(.) &&
      . %in% colnames(df)
  })
  
  if(length(valid_args) == 0){
    return(NULL)
  }
  purrr::as_vector(valid_args, .type = character(1))
}

subset_plot_args <- function(args, method) {
  if(is.null(args)) {
    return(NULL)
  }
  m_args <- method_args[[method]]
  m_req_args <- method_required_args[[method]]
  
  # Only subset the values that exist in args
  valid_args <- args[m_args[m_args %in% names(args)]]
  
  if(length(valid_args) == 0 || !all(m_req_args %in% names(valid_args))) {
    return(NULL)
  }
  valid_args
}

#' @export
print_plot <- function(plot) {
  suppressWarnings(purrr::possibly(print, otherwise = NULL)(plot))
}

