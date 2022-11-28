test_that("download_df works", {
  df <- tibble::tibble(x = 1, y = 2)
  csv_file <- tempfile(fileext = ".csv")
  withr::local_file(csv_file)
  excel_file <- tempfile(fileext = ".xlsx")
  withr::local_file(excel_file)
  
  download_df(NULL, "CSV", csv_file) %>%
    expect_null()
  download_df(df, NULL, csv_file) %>%
    expect_null()
  download_df(df, "a", csv_file) %>%
    expect_null()
  download_df(df, "CSV", NULL) %>%
    expect_null()
  download_df(df, "CSV", csv_file)
  vroom::vroom(csv_file) %>%
    expect_equal(df)
  download_df(df, "Excel", excel_file)
  readxl::read_excel(excel_file) %>%
    expect_equal(df)
})
