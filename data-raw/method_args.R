## code to prepare `method_args` dataset goes here

method_args <- list(
  line = c('x', 'y', 'colour'),
  scatter = c('x', 'y', 'colour', 'size', 'shape'),
  histogram = c('x', 'colour', 'size')
)

usethis::use_data(method_args, overwrite = TRUE)
