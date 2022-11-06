## code to prepare `args_type` dataset goes here

args_type <- list(
  x = 'any',
  y = 'any',
  colour = 'categorical',
  size = 'continuous',
  shape = 'categorical'
)

usethis::use_data(args_type, overwrite = TRUE)
