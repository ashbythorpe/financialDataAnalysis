test_that("download_df works", {
  df <- tibble::tibble(x = 1, y = 2)
  csv_file <- tempfile(fileext = ".csv")
  excel_file <- tempfile(fileext = ".xlsx")
  download_df(NULL, "CSV", csv_file) %>%
    expect_null()
  download_df(df, NULL, csv_file) %>%
    expect_null()
  download_df(df, "a", csv_file) %>%
    expect_null()
  download_df(df, "CSV", NULL) %>%
    expect_null()
  download_df(df, "CSV", csv_file)
  readr::read_csv(csv_file) %>%
    expect_equal(df)
  download_df(df, "Excel", excel_file) %>%
    expect_equal(excel_file)
  readxl::read_excel(excel_file) %>%
    expect_equal(df)
})
