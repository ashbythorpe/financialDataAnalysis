## code to prepare `daily_stock_data` dataset goes here
library(yfR)
library(lubridate)

sp500 <- yf_index_composition("SP500")
symbols <- sp500$ticker

daily_data <- yf_get(symbols,
  first_date = today() - months(6),
  last_date = today(),
  freq_data = "daily"
)

usethis::use_data(daily_stock_data, overwrite = TRUE)
