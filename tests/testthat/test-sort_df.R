test_that("sort_df works", {
  df <- tibble::tibble(x = 10:1, y = letters[c(1:9, 4)])
  sort_df(NULL, "x", F) %>%
    expect_null()
  sort_df(df, colname = NULL) %>%
    expect_equal(df)
  sort_df(df, colname = "x", desc = NULL) %>%
    expect_equal(df)
  sort_df(df, colname = "z", desc = F) %>%
    expect_equal(df)
  sort_df(df, "x", desc = F) %>%
    expect_equal(tibble::tibble(
      x = 1:10, y = letters[c(4, 9:1)]
    ))
  sort_df(df, "x", desc = T) %>%
    expect_equal(df)
  sort_df(df, "y", desc = F) %>%
    expect_equal(tibble::tibble(
      x = c(10, 9, 8, 7, 1, 6, 5, 4, 3, 2),
      y = letters[c(1:4, 4, 5:9)]
    ))
  sort_df(df, "y", desc = T) %>%
    expect_equal(tibble::tibble(
      x = c(2, 3, 4, 5, 6, 7, 1, 8, 9, 10),
      y = letters[c(9:5, 4, 4:1)]
    ))
})
