test_that("data_input_server works", {
  example_invalid_data_path <- tempfile("invalid", fileext = ".csv")
  withr::local_file(example_invalid_data_path)
  example_invalid_data <- tibble::tibble(x = 1, y = 1)
  vroom::vroom_write(example_invalid_data, example_invalid_data_path, ",")
  
  example_valid_data_path <- tempfile("valid", fileext = ".csv")
  withr::local_file(example_valid_data_path)
  example_valid_data <- tibble::tibble(x = c(1,2), y = c(2,1))
  vroom::vroom_write(example_valid_data, example_valid_data_path, ",")
  
  testServer(data_input_server, {
    session$setInputs(combine = FALSE)
    
    expect_equal(final_df(), default_stock_data)
    
    session$setInputs(files = list(datapath = example_invalid_data_path))
    
    expect_equal(final_df(), default_stock_data)
    
    session$setInputs(files = list(datapath = example_valid_data_path))
    
    expect_equal(final_df(), example_valid_data)
    
    session$setInputs(combine = TRUE)
    
    expect_equal(final_df(), dplyr::bind_rows(example_valid_data, default_stock_data))
    
    session$setInputs(reset = 1)
    
    expect_equal(final_df(), default_stock_data)
  })
})

