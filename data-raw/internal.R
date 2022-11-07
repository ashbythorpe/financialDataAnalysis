## code to prepare `internal` dataset goes here

args_type <- list(
  x = 'any',
  y = 'any',
  colour = 'categorical',
  size = 'continuous',
  shape = 'categorical'
)

method_args <- list(
  line = c('x', 'y', 'colour'),
  scatter = c('x', 'y', 'colour', 'size', 'shape'),
  histogram = c('x', 'colour', 'size')
)

method_required_args <- list(
  line = c('x', 'y'),
  scatter = c('x', 'y'),
  histogram = c('x')
)

usethis::use_data(
  args_type,
  method_args,
  method_required_args,
  internal = TRUE, 
  overwrite = TRUE
)
