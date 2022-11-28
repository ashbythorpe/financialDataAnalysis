test_that("input_data works", {
  default_data <- tibble::tibble(
    x = 1:5,
    y = 10:6,
    z = letters[1:5]
  )
  
  example_invalid_data_path <- tempfile("invalid", fileext = ".csv")
  withr::local_file(example_invalid_data_path)
  example_invalid_data <- tibble::tibble(x = 1, y = 1)
  vroom::vroom_write(example_invalid_data, example_invalid_data_path, ",")

  example_invalid_data2_path <- tempfile("invalid2", fileext = ".csv")
  withr::local_file(example_invalid_data2_path)
  example_invalid_data2 <- tibble::tibble(x = c(1,2))
  vroom::vroom_write(example_invalid_data2, example_invalid_data2_path, ",")

  example_valid_data_path <- tempfile("valid", fileext = ".csv")
  withr::local_file(example_valid_data_path)
  example_valid_data <- tibble::tibble(x = c(1,2), y = c(2,1))
  vroom::vroom_write(example_valid_data, example_valid_data_path, ",")

  example_excel_path <- tempfile("valid_excel", fileext = ".xlsx")
  withr::local_file(example_excel_path)
  example_excel <- tibble::tibble(x = c(1,2), y = c(2,1))
  writexl::write_xlsx(example_excel, example_excel_path)
  
  quietly_input_data <- purrr::quietly(input_data)
  input_data(NULL, default_data = default_data, combine = FALSE) %>%
    expect_equal(default_data)
  
  result_1 <- quietly_input_data("aaa", default_data = default_data, combine = FALSE)
  expect_equal(result_1$result, default_data)
  expect_match(result_1$output, "Files were not converted correctly\\.")
  
  result_2 <- quietly_input_data(c("aaa", example_valid_data_path),
                                 default_data = default_data, combine = FALSE)
  expect_equal(result_2$result, example_valid_data)
  expect_match(result_2$output, "Not all files were converted correctly\\.")
  
  result_3 <- quietly_input_data(example_invalid_data_path,
                                 default_data = default_data, combine = FALSE)
  expect_equal(result_3$result, default_data)
  expect_match(result_3$output, "The inputted data frame is not valid\\.")
  
  input_data(example_valid_data_path, default_data = default_stock_data, combine = FALSE) %>%
    expect_equal(example_valid_data)
  input_data(example_excel_path, default_data = default_data, combine = FALSE) %>%
    expect_equal(example_excel)
  input_data(c(example_valid_data_path, example_excel_path), default_data = default_data, combine = FALSE) %>%
    expect_equal(example_valid_data)
  
  input_data(example_valid_data_path, default_data = default_data, combine = TRUE) %>%
    expect_equal(tibble::tibble(
      x = c(1,2,1:5),
      y = c(2,1,10:6),
      z = c(NA, NA, letters[1:5])
    ))
  input_data(example_invalid_data_path, default_data = default_data, combine = TRUE) %>%
    expect_equal(tibble::tibble(
      x = c(1,1:5),
      y = c(1,10:6),
      z = c(NA, letters[1:5])
    ))
})

test_that("read_files works", {
  example_invalid_data_path <- tempfile("invalid", fileext = ".csv")
  example_invalid_data <- tibble::tibble(x = 1, y = 1)
  vroom::vroom_write(example_invalid_data, example_invalid_data_path, ",")

  example_valid_data_path <- tempfile("valid", fileext = ".csv")
  example_valid_data <- tibble::tibble(x = c(1,2), y = c(2,1))
  vroom::vroom_write(example_valid_data, example_valid_data_path, ",")

  example_excel_path <- tempfile("valid_excel", fileext = ".xlsx")
  example_excel <- tibble::tibble(x = c(1,2), y = c(2,1))
  writexl::write_xlsx(example_excel, example_excel_path)
  #files cannot be NULL.
  read_files("aaa") %>%
    expect_null()
  read_files(c("aaa", "bbb")) %>%
    expect_null()
  read_files(c("aaa", example_valid_data_path)) %>%
    expect_equal(list(example_valid_data))
  read_files(c(example_invalid_data_path, example_excel_path)) %>%
    expect_equal(list(
      example_invalid_data, example_excel
    ))
})

test_that("read_file works", {
  example_valid_data_path <- tempfile("valid", fileext = ".csv")
  example_valid_data <- tibble::tibble(x = c(1,2), y = c(2,1))
  vroom::vroom_write(example_valid_data, example_valid_data_path, ",")

  example_excel_path <- tempfile("excel", fileext = ".xlsx")
  writexl::write_xlsx(example_valid_data, example_excel_path)

  example_invalid_excel_path <- tempfile("invalid", fileext = ".xlsx")
  vroom::vroom_write(example_valid_data, example_invalid_excel_path, ",")

  example_invalid_path <- tempfile("invalid2")
  glue::glue_safe("{example_valid_data}") %>%
    {rlang::inject(paste0(!!!.))} %>%
    vroom::vroom_write(example_invalid_path, "")

  read_file(NA) %>%
    expect_null()
  read_file(example_valid_data_path) %>%
    expect_equal(example_valid_data)
  read_file(example_excel_path) %>%
    expect_equal(example_valid_data)
  read_file(example_invalid_path) %>%
    {nrow(.) == 0} %>%
    expect_true()
  read_file(example_invalid_excel_path) %>%
    expect_null()
})

test_that("file_format works", {
  file_format("x.csv") %>%
    expect_equal("Delimited")
  purrr::map(list("x.xls", "x.xlsm", "x.csv.xls", "x.xlsx", "x.xltx", "x.xltm"), file_format) %>%
    purrr::walk(expect_equal, "Excel") #since the return value from expect_equal should not be shown
  file_format("x.aaa") %>%
    expect_equal("not recognised")
  file_format("x.csv.") %>%
    expect_equal("not recognised")
  file_format("x.csv.xls") %>%
    expect_equal("Excel")
})

test_that("combine_if_multiple works", {
  combine_if_multiple(NULL) %>%
    expect_null()
  combine_if_multiple(list(tibble::tibble(x = 1, y = 2))) %>%
    expect_equal(tibble::tibble(x = 1, y = 2))
  combine_if_multiple(list(tibble::tibble(x = 1, y = 2),
                           tibble::tibble(x = 3, z = 4))) %>%
    expect_equal(tibble::tibble(x = c(1,3), y = c(2, NA), z = c(NA, 4)))
})

test_that("combine_two_dfs works", {
  combine_two_dfs(NULL, NULL) %>%
    expect_equal(NULL)
  combine_two_dfs(NULL, 1) %>%
    expect_equal(1)
  combine_two_dfs(2, NULL) %>%
    expect_equal(2)
  combine_two_dfs(tibble::tibble(x = 1, y = 2), tibble::tibble(x = 1, z = 3)) %>%
    expect_equal(tibble::tibble(x = 1, y = 2, z = 3))
  combine_two_dfs(tibble::tibble(x = 1, y = 2), tibble::tibble(x = 2, y = 3)) %>%
    expect_equal(tibble::tibble(x = c(1,2), y = c(2,3)))
  combine_two_dfs(tibble::tibble(x = 1, y = 2), tibble::tibble(z = 3)) %>%
    expect_equal(tibble::tibble(x = 1, y = 2, z = 3))
  combine_two_dfs(tibble::tibble(x = c(1,2,3)), tibble::tibble(y = c(1,2))) %>%
    expect_equal(tibble::tibble(x = c(1,2,3,NA,NA), y = c(NA,NA,NA,1,2)))
  combine_two_dfs(tibble::tibble(x = 1:10), tibble::tibble(x = "1")) %>%
    expect_equal(tibble::tibble(
      x.x = c(1:10, NA),
      x.y = c(rep(NA, 10), "1")
    ))
  combine_two_dfs(tibble::tibble(
    x = 1:10,
    y = 10:1
  ), tibble::tibble(
    x = 1:10,
    y = "a"
  )) %>%
    expect_equal(tibble::tibble(
      x = 1:10,
      y.x = 10:1,
      y.y = "a"
    ))
})

#combine can't be NULL
test_that("combine_if_specified works", {
  default <- tibble::tibble(x = 1, y = 2)
  combine_if_specified(1, default = default, combine = F) %>%
    expect_equal(1)
  combine_if_specified(tibble::tibble(x = 1, z = 3), default = default, combine = T) %>%
    expect_equal(tibble::tibble(x = 1, z = 3, y = 2))
  combine_if_specified(NULL, default = default, combine = F) %>%
    expect_equal(NULL)
  combine_if_specified(NULL, default = default, combine = T) %>%
    expect_equal(default)
})

test_that("transform_df works", {
  default <- tibble::tibble(x = 1, y = 2)
  transform_df(NULL, default = default) %>%
    expect_equal(NULL)
  transform_df(tibble::tibble(x = 1, y = 2), default = default) %>%
    expect_equal(tibble::tibble(x = 1, y = 2))
  transform_df(tibble::tibble(x = c(1,2,3,3,NA), y = 1, z = "1"), default = default) %>%
    expect_equal(tibble::tibble(x = c(1,2,3,NA), y = 1, z = 1))
  transform_df(tibble::tibble(x = "a", y = "b"), default = default) %>%
    expect_equal(NULL)
  transform_df(tibble::tibble(x = NA_real_, y = NA, z = "a"), default = default) %>%
    expect_equal(NULL)
  transform_df(tibble::tibble(x = NA, y = NA), default = default) %>%
    expect_equal(NULL)
  transform_df(tibble::tibble(x = c(1,1,1), y = c(1,1,1)), default = default) %>%
    expect_equal(tibble::tibble(x = 1, y = 1))
  valid_df <- tibble::tibble(x = c(1,2,3), y = c(2,3,4))
  transform_df(valid_df, default = default) %>%
    expect_equal(valid_df)
})

test_that("transform_col works", {
  transform_col(c(1, 2, 3, 4)) %>%
    expect_equal(c(1, 2, 3, 4))
  transform_col(c(TRUE, FALSE, TRUE, FALSE)) %>%
    expect_equal(c(1, 0, 1, 0))
  transform_col(c("a", "a")) %>%
    expect_equal(c("a", "a"))
  transform_col(c("a", "1")) %>%
    expect_equal(c(NA, 1))
  transform_col(c("a", "b", "1")) %>%
    expect_equal(c("a", "b", "1"))
  transform_col(c("1", "2")) %>%
    expect_equal(c(1, 2))
})

#files can't be NULL
test_that("get_error works", {
  get_error(1, NULL, 1) %>%
    expect_equal(list(fatal = "Files were not converted correctly.", nonfatal = ""))
  get_error(list(1,2), list(1), 1) %>%
    expect_equal(list(fatal = "", nonfatal = "Not all files were converted correctly."))
  get_error(1, 1, NULL) %>%
    expect_equal(list(fatal = "The data does not contain any scorable columns.", nonfatal = ""))
  get_error(1, 1, tibble::tibble(x = c(1,2))) %>%
    expect_equal(list(fatal = "The inputted data frame is not valid.", nonfatal = ""))
  get_error(1, 1, tibble::tibble(x = 1, y = 2)) %>%
    expect_equal(list(fatal = "The inputted data frame is not valid.", nonfatal = ""))
  get_error(1, 1, tibble::tibble(x = c(1,2), y = c(2,3))) %>%
    expect_equal(list(fatal = "", nonfatal = ""))
})

test_that("validate_df works", {
  default <- tibble::tibble(x = 1, y = 2)
  validate_df(NULL, default = default, error = "") %>%
    expect_equal(default)
  validate_df(tibble::tibble(x = 1), default = default, error = "error") %>%
    expect_equal(default)
  validate_df(tibble::tibble(x = 1), default = default, error = "") %>%
    expect_equal(tibble::tibble(x = 1))
})
