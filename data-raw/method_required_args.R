## code to prepare `method_required_args` dataset goes here

method_required_args <- list(
  line = c('x', 'y'),
  scatter = c('x', 'y'),
  histogram = c('x')
)

usethis::use_data(method_required_args, overwrite = TRUE)
