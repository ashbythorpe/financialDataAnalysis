test_that("predict_price + plot_predictions works", {
  date <- lubridate::ymd("2023", truncated = 2)
  predict_price("GOOGL", start_date = date, date + lubridate::days(60)) %>%
    plot_predictions()
})
