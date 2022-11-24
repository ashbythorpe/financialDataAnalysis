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
#' @seealso [print_plot()]
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
  
  if(is.null(method)) {
    return(NULL)
  }
  
  args_result <- switch(
    method,
    line = validate_line_args(plot_args, df),
    scatter = validate_scatter_args(plot_args, df),
    histogram = validate_histogram_args(plot_args, df),
    smooth = validate_smooth_args(plot_args, df),
    area = validate_area_args(plot_args, df),
    hex = validate_area_args(plot_args, df),
    NULL
  )
  
  if(is.null(args_result)){
    return(NULL)
  }
  
  args <- args_result$aes
  other <- args_result$other
  
  # The plot only errors when it is printed: assign the result of this function
  # to a variable and it won't produce an error.
  rlang::inject(
    create_plot(df, method = method, args = args, !!!other)
  )
}

validate_plotting_method <- function(x){
  if(is.null(x) || !x %in% c(
    "line", "scatter", "histogram", "smooth", "area", "hex"
  )){
    return(NULL)
  }
  x
}

validate_line_args <- function(args, data) {
  # Remove NA and NULL args
  args_v <- remove_invalid_args(args)
  # Get args that describe columns
  args_aes <- get_aes_args(args_v, data)
  
  accepted_args <- c("x", "y", "alpha", "colour", "linewidth")
  
  valid_aes <- args_aes[names(args_aes) %in% accepted_args]
  
  # Check all required arguments were specified
  if(!all(c("x", "y") %in% names(valid_aes))) {
    return(NULL)
  }
  
  # Get all other, constant arguments
  valid_other <- list()
  if("opacity" %in% names(args_v) && !"alpha" %in% names(valid_aes) && 
     is.numeric(args_v$opacity)) {
    valid_other$alpha <- args_v$opacity
  }
  if("size" %in% names(args_v) && !"linewidth" %in% names(valid_aes) && 
     is.numeric(args_v$size)) {
    valid_other$linewidth <- args_v$size
  }
  
  list(
    aes = valid_aes,
    other = valid_other
  )
}

validate_scatter_args <- function(args, data) {
  # Remove NA and NULL args
  args_v <- remove_invalid_args(args)
  # Get args that describe columns
  args_aes <- get_aes_args(args_v, data)
  
  accepted_args <- c("x", "y", "alpha", "colour", "shape", "size")
  
  valid_aes <- args_aes[names(args_aes) %in% accepted_args]
  
  # Check all required arguments were specified
  if(!all(c("x", "y") %in% names(valid_aes))) {
    return(NULL)
  }
  
  # Get all other, constant arguments
  valid_other <- list()
  if("opacity" %in% names(args_v) && !"alpha" %in% names(valid_aes) && 
     is.numeric(args_v$opacity)) {
    valid_other$alpha <- args_v$opacity
  }
  if("pointsize" %in% names(args_v) && !"size" %in% names(valid_aes) && 
     is.numeric(args_v$pointsize)) {
    valid_other$size <- args_v$pointsize
  }
  if(isTRUE(args_v$jitter)) {
    valid_other$position <- "jitter"
  }
  
  list(
    aes = valid_aes,
    other = valid_other
  )
}

validate_histogram_args <- function(args, data) {
  # Remove NA and NULL args
  args_v <- remove_invalid_args(args)
  # Get args that describe columns
  args_aes <- get_aes_args(args_v, data)
  
  accepted_args <- c("x", "y", "alpha", "colour", "fill")
  
  valid_aes <- args_aes[names(args_aes) %in% accepted_args]
  
  # Check all required arguments were specified
  if(!any(c("x", "y") %in% names(valid_aes)) || 
     all(c("x", "y") %in% names(valid_aes))) {
    return(NULL)
  }
  
  # Get all other, constant arguments
  valid_other <- list()
  if("opacity" %in% names(args_v) && !"alpha" %in% names(valid_aes) && 
     is.numeric(args_v$opacity)) {
    valid_other$alpha <- args_v$opacity
  }
  if("bins" %in% names(args_v) && is.numeric(args_v$bins)) {
    valid_other$bins <- args_v$bins
  }
  
  list(
    aes = valid_aes,
    other = valid_other
  )
}

validate_smooth_args <- function(args, data) {
  # Remove NA and NULL args
  args_v <- remove_invalid_args(args)
  # Get args that describe columns
  args_aes <- get_aes_args(args_v, data)
  
  accepted_args <- c("x", "y", "alpha")
  
  valid_aes <- args_aes[names(args_aes) %in% accepted_args]
  
  # Check all required arguments were specified
  if(!all(c("x", "y") %in% names(valid_aes))) {
    return(NULL)
  }
  
  # Get all other, constant arguments
  valid_other <- list()
  if("opacity" %in% names(args_v) && !"alpha" %in% names(valid_aes) && 
     is.numeric(args_v$opacity)) {
    valid_other$alpha <- args_v$opacity
  }
  if("span" %in% names(args_v) && is.numeric(args_v$span)) {
    valid_other$span <- args_v$span
  }
  
  list(
    aes = valid_aes,
    other = valid_other
  )
}

validate_area_args <- function(args, data) {
  # Remove NA and NULL args
  args_v <- remove_invalid_args(args)
  # Get args that describe columns
  args_aes <- get_aes_args(args_v, data)
  
  accepted_args <- c("x", "y", "alpha", "colour", "fill", "linewidth")
  
  valid_aes <- args_aes[names(args_aes) %in% accepted_args]
  
  # Check all required arguments were specified
  if(!all(c("x", "y") %in% names(valid_aes))) {
    return(NULL)
  }
  
  # Get all other, constant arguments
  valid_other <- list()
  if("opacity" %in% names(args_v) && !"alpha" %in% names(valid_aes) && 
     is.numeric(args_v$opacity)) {
    valid_other$alpha <- args_v$opacity
  }
  if("size" %in% names(args_v) && !"linewidth" %in% names(valid_aes) && 
     is.numeric(args_v$size)) {
    valid_other$linewidth <- args_v$size
  }
  
  list(
    aes = valid_aes,
    other = valid_other
  )
}

validate_hex_args <- function(args, data) {
  # Remove NA and NULL args
  args_v <- remove_invalid_args(args)
  # Get args that describe columns
  args_aes <- get_aes_args(args_v, data)
  
  accepted_args <- c("x", "y", "alpha", "colour", "fill")
  
  valid_aes <- args_aes[names(args_aes) %in% accepted_args]
  
  # Check all required arguments were specified
  if(!all(c("x", "y") %in% names(valid_aes))) {
    return(NULL)
  }
  
  # Get all other, constant arguments
  valid_other <- list()
  if("opacity" %in% names(args_v) && !"alpha" %in% names(valid_aes) && 
     is.numeric(args_v$opacity)) {
    valid_other$alpha <- args_v$opacity
  }
  if("bins" %in% names(args_v) && is.numeric(args_v$bins)) {
    valid_other$bins <- args_v$bins
  }
  
  list(
    aes = valid_aes,
    other = valid_other
  )
}

create_plot <- function(df, args, method, ...){
  # Fix alpha aesthetic for plotly
  extra_layers <- list()
  if("alpha" %in% names(args) && is.character(args["alpha"])) {
    alpha <- call("factor", rlang::data_sym(args["alpha"]))
    
    other_args <- args[names(args) != "alpha"]
    
    alpha_col <- df[[args["alpha"]]]
    if(is.numeric(alpha_col)) {
      scaled_col <- alpha_col/max(alpha_col, na.rm = TRUE)
      
      aesthetics <- other_args %>%
        rlang::data_syms() %>%
        ggplot2::aes(,,!!!., alpha = !!alpha)
      
      extra_layers <- ggplot2::scale_alpha_manual(
        guide = "none", values = unique(scaled_col)
      )
    } else {
      aesthetics <- other_args %>%
        rlang::data_syms() %>%
        ggplot2::aes(,,!!!., alpha = !!alpha)
    }
  } else {
    aesthetics <- args %>%
      rlang::data_syms() %>%
      ggplot2::aes(,,!!!.)
  }
  
  # Use frequency density rather than count
  if(method == "histogram") {
    if("x" %in% names(args)) {
      histogram_aes <- ggplot2::aes(y = ggplot2::after_stat(density))
    } else {
      histogram_aes <- ggplot2::aes(x = ggplot2::after_stat(density))
    }
  }
  
  layer <- 
    switch(method,
      line = ggplot2::geom_line(...),
      scatter = ggplot2::geom_point(...),
      histogram = ggplot2::geom_histogram(histogram_aes, ...),
      smooth = ggplot2::geom_smooth(...),
      area = ggplot2::geom_area(...),
      hex = ggplot2::geom_hex(...),
      NULL
    )
  
  ggplot2::ggplot(data = df, aesthetics) +
    layer +
    extra_layers
}

remove_invalid_args <- function(x) {
  purrr::discard(x, ~ {
    is.null(.) || is.na(.)
  })
}

get_aes_args <- function(x, data) {
  valid <- purrr::keep(x, ~ {
    is.character(.) && . %in% colnames(data)
  })
  
  if(length(valid) == 0){
    return(NULL)
  }
  
  purrr::as_vector(valid, .type = character(1))
}

#' Safely print a plot
#' 
#' Suppress all errors and warnings while printing a plot.
#' 
#' @param plot The plot to print.
#' 
#' @seealso [custom_plot()]
#' 
#' @returns `plot`
#' 
#' @export
print_plot <- function(plot) {
  suppressWarnings(purrr::possibly(print, otherwise = NULL)(plot))
}

build_plot <- function(plot) {
  suppressWarnings(purrr::possibly(plotly::plotly_build, otherwise = NULL)(plot))
}





# OLD FUNCTIONS
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
