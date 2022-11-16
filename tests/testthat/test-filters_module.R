test_that("filter_server works", {
  testServer(filter_server, {
    expect_equal(row(), tibble::tibble_row(
      min = NA_real_,
      max = NA_real_
    ))
    
    session$setInputs(max = 2, min = 3)
    
    expect_equal(row(), tibble::tibble_row(
      min = NA_real_,
      max = NA_real_
    ))
    
    session$setInputs(max = 3, min = 2)
    
    expect_equal(row(), tibble::tibble_row(
      min = 2,
      max = 3
    ))
  })
})
