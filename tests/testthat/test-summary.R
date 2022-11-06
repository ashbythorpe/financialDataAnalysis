test_that("col_summary_val works", {
  col_summary_val(c(1,2,3,4,NA), mean, "Mean") %>%
    expect_equal("Mean: 2.5")
})

test_that("col_summary works", {
  col_summary(c(1,2,3,4,NA)) %>%
    expect_equal("Mean: 2.5, Median: 2.5, Minimum: 1, Maximum: 4, Standard deviation: 1.29")
})

test_that("score_summary works", {
  score_summary(1:100, score_type = NA_character_, lb = 0, ub = 100, centre = NA, inverse = NA,
                exponential = F, logarithmic = NA, magnitude = NA, custom_args = NA) %>%
    expect_null()
  score_summary(1:100, score_type = "Linear", lb = 0, ub = 100, centre = NULL, inverse = NULL,
                exponential = F, logarithmic = NULL, magnitude = NULL, custom_args = NULL) %>%
    vdiffr::expect_doppelganger(title = "Score summary")
})

test_that("stock_summary works", {
  stock_summary(tibble::tibble(
    x = c(1,2,3,4,5)
  ), 3) %>%
    expect_equal(tibble::tibble(
      x = 3
    ))
})
