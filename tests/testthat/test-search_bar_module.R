test_that("search_bar_server works", {
  testServer(search_bar_server, {
    expect_equal(stock(), NULL)
    
    session$setInputs(stock = "aaaaa")
    
    expect_equal(stock(), NULL)
    
    session$setInputs(stock = "GOOGL")
    
    expect_equal(stock(), "GOOGL")
  })
})
