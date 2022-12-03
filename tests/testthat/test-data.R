test_that("default_stock_data is valid", {
  expect_s3_class(default_stock_data, c("data.frame", "tbl_df"))
  expect_equal(nrow(default_stock_data), 
               length(unique(default_stock_data$symbol)))
  expect_equal(anyDuplicated(colnames(default_stock_data)), 0)
  # No column names are found within another
  purrr::none(colnames(default_stock_data), ~ {
    all(stringr::str_detect(
      colnames(default_stock_data),
      paste0("\\Q", ., "\\E")
    ))
  }) %>%
    expect_true()
  expect_equal(colnames(default_stock_data)[1], "symbol")
  expect_true("GOOGL" %in% default_stock_data$symbol)
  purrr::every(default_stock_data, ~ {
    vctrs::vec_is(., character()) || vctrs::vec_is(., double()) || 
      vctrs::vec_is(., integer())
  }) %>%
    expect_true()
})
