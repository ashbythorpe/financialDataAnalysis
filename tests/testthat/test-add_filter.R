test_that("add_filter works", {
  df <- tibble::tibble(x = c(1, 2, 3), y = c("a", "b", "c"))
  add_filter(filters_init, colname = NULL, data = df) %>%
    expect_equal(filters_init)
  add_filter(filters_init, colname = "z", data = df) %>%
    expect_equal(filters_init)
  add_filter(filters_init, colname = "x", data = df) %>%
    expect_equal(tibble::tibble(
      type = "numeric", colname = "x", pattern = NA_character_,
      min = 1, max = 3
    ))
  add_filter(filters_init, colname = "y", data = df) %>%
    expect_equal(tibble::tibble(
      type = "character", colname = "y", pattern = "", min = NA_real_,
      max = NA_real_
    ))
})

test_that("edit_character_filter works", {
  filters <- tibble::tibble(
    type = "character", colname = "y", pattern = "", min = NA_real_, max = NA_real_
  )
  edit_character_filter(filters, 1, pattern = NULL) %>%
    expect_equal(filters)
  edit_character_filter(filters, 1, pattern = "aaa") %>%
    expect_equal(tibble::tibble(
      type = "character", colname = "y", pattern = "aaa", min = NA_real_,
      max = NA_real_
    ))
})

test_that("edit_numeric_filter works", {
  filters <- tibble::tibble(
    type = "numeric", colname = "x", pattern = NA_character_,
    min = 1, max = 3
  )
  edit_numeric_filter(filters, 1, min = NULL, max = 10) %>%
    expect_equal(filters)
  edit_numeric_filter(filters, 1, min = 5, max = NULL) %>%
    expect_equal(filters)
  edit_numeric_filter(filters, 1, min = 5, max = 10) %>%
    expect_equal(tibble::tibble(
      type = "numeric", colname = "x", pattern = NA_character_,
      min = 5, max = 10
    ))
})

test_that("edit_filter works", {
  filters <- tibble::tibble(
    type = c("numeric", "character"), colname = c("x", "y"), pattern = c(NA, ""),
    min = c(1, NA), max = c(3, NA)
  )
  edit_filter(filters, 2, pattern = "a") %>%
    expect_equal(tibble::tibble(
      type = c("numeric", "character"), colname = c("x", "y"), pattern = c(NA, "a"),
      min = c(1, NA), max = c(3, NA)
    ))
  edit_filter(filters, 1, min = 1, max = 4, pattern = "") %>%
    expect_equal(tibble::tibble(
      type = c("numeric", "character"), colname = c("x", "y"), pattern = c(NA, ""),
      min = c(1, NA), max = c(4, NA)
    ))
  edit_filter(filters, 3, min = 1, max = 4, pattern = "") %>%
    expect_equal(filters)
})

test_that("remove_filter works", {
  filters <- tibble::tibble(
    type = c("numeric", "character"), colname = c("x", "y"), pattern = c(NA, ""),
    min = c(1, NA), max = c(3, NA)
  )
  remove_filter(filters, 3) %>%
    expect_equal(filters)
  remove_filter(filters, 2) %>%
    expect_equal(tibble::tibble(
      type = "numeric", colname = "x", pattern = NA_character_,
      min = 1, max = 3
    ))
})
