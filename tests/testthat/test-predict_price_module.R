test_that("predict_price_server works", {
  testServer(predict_price_server, args = list(
    stock = reactive("GOOGL")
  ), {
    session$setInputs(
      frequency = "daily",
      dates_daily = c(
        lubridate::today(), lubridate::today() + 10
      )
    )
    
    vdiffr::expect_doppelganger(title = "price_predictions", plot())
  })
})
