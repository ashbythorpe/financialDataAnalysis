test_that("create_filter_server works", {
  testServer(create_filter_server, args = list(
    data = reactive(tibble::tibble(
      x = 1:10,
      y = letters[1:10]
    ))
  ), {
    expect_equal(valid_colnames(), "x")
    
    session$setInputs(colname = "y", create = 1)

    expect_equal(add(), NULL)

    session$setInputs(colname = "x")

    expect_equal(add(), NULL)
    
    session$setInputs(create = 1)
    
    expect_equal(add(), "x")
  })
})
