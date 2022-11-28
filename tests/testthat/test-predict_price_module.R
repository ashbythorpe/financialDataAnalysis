test_that("predict_price_server works", {
  skip_on_ci()
  withr::local_seed(42)
  
  testServer(predict_price_server, args = list(
    stock = reactive("GOOGL"), interactive = reactive(TRUE)
  ), {
    session$setInputs(
      frequency = "daily",
      dates_daily = c(
        lubridate::today(), lubridate::today() + 10
      ),
      predict = 1
    )
    
    vdiffr::expect_doppelganger(title = "price_predictions", plot())
  })
})
