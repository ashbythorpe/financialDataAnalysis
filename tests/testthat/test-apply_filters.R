test_that("filter_column works", {
  filter_column(c("aaaaaaaa", "aaaa.1www", "a.", ".1", "a1", "aa1", "", NA),
                type = "string", pattern = "a.1", min = NA, max = NA) %>%
    expect_equal(c(F, T, F, F, F, F, F, NA))
  filter_column(c("aaaaaaaa", "aaaa.1www", "a.", ".1", "a1", "aa1", "", NA),
                type = "string", pattern = "", min = NA, max = NA) %>%
    expect_equal(c(rep(T, 7), NA))
  filter_column(c("aaaaaaaa", "aaaa.1www", "a.", ".1", "a1", "aa1", "", NA),
                type = "string", pattern = "b", min = NA, max = NA) %>%
    expect_equal(c(rep(F, 7), NA))
  filter_column(c(1:10, NA), type = "numeric", pattern = NA, min = 3, max = 7) %>%
    expect_equal(c(F, F, T, T, T, T, T, F, F, F, NA))
  filter_column(c(1:10, NA), type = "numeric", pattern = NA, min = 1, max = 10) %>%
    expect_equal(c(rep(T, 10), NA))
  filter_column(c(1:10, NA), type = "numeric", pattern = NA, min = 11, max = 12) %>%
    expect_equal(c(rep(F, 10), NA))
  filter_column(c(1:10, NA), type = "numeric", pattern = NA, min = -1, max = 0) %>%
    expect_equal(c(rep(F, 10), NA))
  filter_column(c(1:10, NA), type = "numeric", pattern = NA, min = 6, max = 5) %>%
    expect_equal(c(rep(F, 10), NA))
})

test_that("apply_filters works", {
  df <- tibble::tibble(x = c(1:7, NA), y = c("aaaaaaaa", "aaaa.1www", "a.", ".1", 
                                             "a1", "aa1", "", NA))
  apply_filters(df, tibble::tibble()) %>%
    expect_equal(df)
  apply_filters(df, tibble::tribble(
    ~type, ~colname, ~pattern, ~min, ~max,
    "string", "y", "a", NA, NA,
    "numeric", "x", NA, 2, 8
  )) %>%
    expect_equal(tibble::tibble(
      x = c(2,3,5,6),
      y = c("aaaa.1www", "a.", "a1", "aa1")
    ))
  apply_filters(df, tibble::tibble(
    type = "numeric",
    colname = "x",
    pattern = NA_character_,
    min = 11,
    max = 12
  )) %>%
    expect_null()
  apply_filters(df, tibble::tibble(
    type = "string",
    colname = "y",
    pattern = "",
    min = NA,
    max = NA
  )) %>%
    expect_equal(
      stats::na.omit(df), ignore_attr = T
    )
})
