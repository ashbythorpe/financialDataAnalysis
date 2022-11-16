test_that("custom_row_server works", {
  testServer(custom_row_server, {
    session$setInputs(x = 1, y = 100)
    
    expect_equal(row(), tibble::tibble_row(
      x = 1, y = 1
    ))
    
    session$setInputs(y = -1)
    
    expect_equal(row(), tibble::tibble_row(
      x = 1, y = 0
    ))
    
    session$setInputs(y = 0.5)
    
    expect_equal(row(), tibble::tibble_row(
      x = 1, y = 0.5
    ))
  })
})


