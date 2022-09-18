test_that("score_linear works", {
  score_linear(NULL, lb = 0, ub = 1) %>%
    expect_equal(NA)
  score_linear(1:100, lb = 20, ub = 80) %>%
    expect_equal(c(rep(0, 20), (21:79-20)/60, rep(1,21)))
  score_linear(1:100, lb = 80, ub = 20) %>%
    expect_equal(c(rep(1, 20), 1-(21:79-20)/60, rep(0,21)))
  score_linear(1:100, lb = -20, ub = 80) %>%
    expect_equal(c(21:99/100, rep(1,21)))
  score_linear(1:100, lb = 20, ub = 120) %>%
    expect_equal(c(rep(0, 20), 1:80/100))
  score_linear(1:100, lb = -20, ub = -10) %>%
    expect_equal(rep(1,100))
  score_linear(1:100, lb = 110, ub = 120) %>%
    expect_equal(rep(0, 100))
})

#lb <= centre <= ub
test_that("score_peak works", {
  score_peak(NULL, lb = 0, ub = 10, centre = 5, inverse = F) %>%
    expect_equal(NA)
  score_peak(1:100, lb = 20, ub = 70, centre = 50, inverse = F) %>%
    expect_equal(c(rep(0, 20), 1:30/30, 1 - 1:20/20, rep(0, 30)))
  score_peak(1:100, lb = 20, ub = 70, centre = 50, inverse = T) %>%
    expect_equal(1 - c(rep(0, 20), 1:30/30, 1 - 1:20/20, rep(0, 30)))
  score_peak(1:100, lb = -20, ub = 70, centre = -10, inverse = F) %>%
    expect_equal(score_linear(1:100, ub = -10, lb = 70))
  score_peak(1:100, lb = 20, ub = 120, centre = 110, inverse = F) %>%
    expect_equal(score_linear(1:100, lb = 20, ub = 110))

})

test_that("format_coords works", {
  format_coords(tibble::tibble(x = 1:5, y = 10:6)) %>%
    expect_equal(tibble::tibble(x = 1:4, y = 10:7, next_x = 2:5, next_y = 9:6))
})

#coordinates cannot be NULL.
test_that("coordinate_section_score works", {
  score_coord_section(1:100, x = 20, y = 0.5, next_x = 60, next_y = 0.4) %>%
    expect_equal(c(rep(NA, 19), 40:0/(40*10) + 0.4, rep(NA, 40)))
  score_coord_section(1:100, x = 20, y = 0.4, next_x = 60, next_y = 0.5) %>%
    expect_equal(c(rep(NA, 19), 0:40/(40*10) + 0.4, rep(NA, 40)))
  score_coord_section(1:100, x = 20, y = 0.2, next_x = 20, next_y = 0.5) %>%
    expect_equal(c(rep(NA, 19), 0.5, rep(NA, 80)))
})

test_that("combine_score_section works", {
  #unnecessary test but good rule for the function to follow.
  combine_score_sections(list(
    c(1,2,3,NA,NA,NA,NA,NA,NA,NA),
    c(NA,NA,NA,4,5,6,NA,NA,NA,NA),
    c(NA,NA,NA,NA,NA,NA,NA,NA,NA,10),
    c(NA,NA,NA,NA,NA,NA,7,8,9,NA)
  )) %>%
    expect_equal(1:10)
  combine_score_sections(list(
    c(1,2,NA),
    c(NA,2,3)
  )) %>%
    expect_equal(1:3)
  combine_score_sections(list(
    c(1,2,3,NA,NA),
    c(NA,NA,4,NA,NA),
    c(NA,NA,4,5,6)
  )) %>%
    expect_equal(c(1,2,4,5,6))
})

test_that("score_custom works", {
  score_custom(NULL, tibble::tibble(x = 1, y = 1)) %>%
    expect_equal(NA)
  score_custom(
    1:100,
    tibble::tribble(
      ~x, ~y,
      10, 0.4,
      20, 0.9,
      50, 0,
      60, 0.2,
      90, 0.5
    )
  ) %>%
    expect_equal(c(rep(NA, 9), 0:10/20 + 0.4, 0.9 - 1:30/(30/0.9), 1:10/50,
                   1:30/(30/0.3) + 0.2, rep(NA, 10)))
  score_custom(
    1:100,
    tibble::tribble(
      ~x, ~y,
      -10, 0.4,
      20, 0.9,
      50, 0,
      50, 0.5,
      90, 0.5
    )
  ) %>%
    expect_equal(c(11:30/60 + 0.4, 0.9 - 1:29/(30/0.9), rep(0.5, 41), rep(NA, 10)))
})

test_that("transform_exponential works", {
  transform_exponential(NULL, exponential = F, logarithmic = F, magnitude = 1) %>%
    expect_equal(NA)
  x <- 0:100/100
  transform_exponential(x, exponential = F, logarithmic = NA, magnitude = NA) %>%
    expect_equal(x)
  transform_exponential(x, exponential = T, logarithmic = F, magnitude = 1) %>%
    {. >= x} %>%
    sum() %>%
    expect_equal(2) #0 and 1
  transform_exponential(x, exponential = T, logarithmic = F, magnitude = 0) %>%
    expect_equal(x)
  transform_exponential(x, exponential = T, logarithmic = F, magnitude = 100000) %>%
    expect_equal(c(rep(0,100), 1))
  transform_exponential(x, exponential = T, logarithmic = F, magnitude = -100000) %>%
    expect_equal(c(0, rep(1,100)))
  transform_exponential(x, exponential = T, logarithmic = T, magnitude = 1) %>%
    expect_equal(transform_exponential(x, exponential = T, logarithmic = F, 
                                       magnitude = -1))
})

test_that("score_column works", {
  score_column(1:100, score_type = NA_character_, lb = 0, ub = 100, centre = NA, 
               inverse = NA,
               exponential = F, logarithmic = NA, magnitude = NA, custom_args = NA) %>%
    expect_equal(1:100/100)
  score_column(1:100, score_type = "Linear", lb = 0, ub = 100, centre = NA, 
               inverse = NA,
               exponential = F, logarithmic = NA, magnitude = NA, custom_args = NA) %>%
    expect_equal(1:100/100)
})

test_that("apply_scores works", {
  df <- tibble::tibble(
    x = 1:100,
    y = 101:200,
    z = seq(200, 1, -2)
  )
  scores_with_scores <-
    tibble::tribble(
      ~score_type, ~colname, ~score_name, ~weight, ~lb, ~ub, ~centre, ~inverse, ~exponential, ~logarithmic, ~magnitude, ~custom_args,
      "Linear", "x", "Score 1", 1, 20, 80, NA, NA, F, NA, NA, NA,
      "Linear", "x", "Score 2", 1, -20, 120, NA, NA, T, F, 1, NA,
      "Peak", "y", "Score 3", 2, 10, 80, 30, F, F, NA, NA, NA,
      "Peak", "z", "Score 4", 3, 10, 120, 90, T, T, T, 2, NA,
      "Custom coordinates", "y", "Score 5", 5, NA, NA, NA, NA, F, NA, NA, tibble::tibble(
        x = c(10, 30, 50, 70, 110),
        y = c(1, 0.2, 0.4, 0.9, 0.1)
      ),
      "Linear", "a", "Score 6", 1, 20, 80, NA, NA, F, NA, NA, NA
    )
  validate_results <- function(x){
    all(!is.na(x$`Score 1`)) &&
      all(!is.na(x$`Score 2`)) &&
      all(x$`Score 3` == 0) &&
      all(is.numeric(x$`Score 4`)) &&
      sum(is.na(x$`Score 5`)) == 90 &&
      all(is.na(x$`Score 6`))
  }
  scores2 <-
    tibble::tribble(
      ~score_type, ~colname, ~score_name, ~weight, ~lb, ~ub, ~centre, ~inverse, ~exponential, ~logarithmic, ~magnitude, ~custom_args,
      "Linear", "x", "x", 1, 20, 80, NA, NA, F, NA, NA, NA
    )

  apply_scores(df, scores) %>%
    expect_equal(df)
  apply_scores(df, scores_with_scores) %>%
    validate_results() %>%
    expect_true()
  apply_scores(df, scores2) %>%
    names() %>%
    {identical(unique(.), .)} %>%
    expect_true()
})
