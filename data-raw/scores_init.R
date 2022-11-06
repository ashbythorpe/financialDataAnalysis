## code to prepare `scores_init` dataset goes here

scores_init <- tibble::tibble(
  score_type = character(),
  colname = character(),
  score_name = character(),
  weight = numeric(),
  lb = numeric(),
  ub = numeric(),
  centre = numeric(),
  inverse = logical(),
  exponential = logical(),
  logarithmic = logical(),
  magnitude = numeric(),
  custom_args = list(), # list column of tibbles
  .rows = 0
)

usethis::use_data(scores_init, overwrite = TRUE)
