## code to prepare `filters_init` dataset goes here

filters_init <- tibble::tibble(
  type = character(),
  colname = character(),
  pattern = character(),
  min = numeric(),
  max = numeric(),
  .rows = 0
)

usethis::use_data(filters_init, overwrite = TRUE)
