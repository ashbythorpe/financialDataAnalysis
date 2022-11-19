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

