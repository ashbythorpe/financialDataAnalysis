# method_fun <- function(){
#   list(
#     line = ggplot2::geom_line(),
#     scatter = ggplot2::geom_point(),
#     histogram = ggplot2::geom_histogram()
#   )
# }
# 
# method_args <- function(){
#   list(
#     line = c('x', 'y', 'colour'),
#     scatter = c('x', 'y', 'colour', 'size', 'shape'),
#     histogram = c('x', 'y', 'colour', 'size')
#   )
# }
# 
# method_required_args <- function(){
#   list(
#     line = c('x', 'y'),
#     scatter = c('x', 'y'),
#     histogram = c('x', 'y')
#   )
# }
# 
# args_type <- function(){
#   list(
#     x = 'any',
#     y = 'any',
#     colour = 'categorical',
#     size = 'continuous',
#     shape = 'categorical'
#   )
# }

custom_plot <- function(df, plotting_method, ...){
  plot_args <- rlang::list2(...)
  valid_plot_args <- validate_plot_args(plot_args, df)
  layer <- validate_plotting_method(plotting_method)
  if(is.null(valid_plot_args) || is.null(layer)){
    return(NULL)
  }
  
  plot <- safely_create_plot(df, valid_plot_args, layer)
  # The actual printing of ggplot objects seems to often be where any errors occur
  purrr::possibly(print, otherwise = NULL)(plot)
}

create_plot <- function(df, args, layer){
  args %>%
    purrr::as_vector(.type = character(1)) %>%
    rlang::parse_exprs() %>%
    ggplot2::aes(!!!.) %>%
    ggplot2::ggplot(data = df) +
    layer
}

safely_create_plot <- purrr::possibly(create_plot, otherwise = NULL)

validate_plotting_method <- function(x){
  if(is.null(x) || !x %in% names(method_fun)){
    return(NULL)
  }
  # make sure the latest version of the function is executed.
  method_fun[[x]] %>%
    rlang::exec(.env = rlang::ns_env("ggplot2"))
}

validate_plot_args <- function(plot_args, df){
  valid_plot_args <- purrr::keep(plot_args, ~ {
    !is.null(.) &&
      . %in% colnames(df)
  })
  if(length(valid_plot_args) == 0){
    return(NULL)
  }
  valid_plot_args
}
