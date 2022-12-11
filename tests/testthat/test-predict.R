test_that("predict_price + plot_predictions works", {
  skip_on_ci()
  withr::local_seed(42)

  date <- lubridate::ymd("2022-8", truncated = 1)
  predict_price("GOOGL", start_date = date, end_date = date + months(6)) %>%
    plot_predictions() %>%
    vdiffr::expect_doppelganger(title = "Daily predictions")

  date2 <- lubridate::ymd("2022", truncated = 2)
  predict_price("GOOGL",
    start_date = date2, end_date = date2 + months(16),
    freq = "monthly"
  ) %>%
    plot_predictions() %>%
    vdiffr::expect_doppelganger(title = "Monthly predictions")
})
