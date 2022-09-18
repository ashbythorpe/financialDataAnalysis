test_that("favourite_stock works", {
  df <- tibble::tibble(x = 1:10, favourite = F)
  favourite_stock(NULL, 1) %>%
    expect_null()
  favourite_stock(df, NULL) %>%
    expect_equal(df)
  favourite_stock(df, 11) %>%
    expect_equal(df)
  favourite_stock(df, 5) %>%
    expect_equal(tibble::tibble(
      x = 1:10,
      favourite = c(F, F, F, F, T, F, F, F, F, F)
    ))
})
