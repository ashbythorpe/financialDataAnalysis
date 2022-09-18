## code to prepare `monthly_stock_data` dataset goes here
library(yfR)
library(lubridate)

sp500 <- yf_index_composition("SP500")
symbols <- sp500$ticker

monthly_data <- yf_get(symbols,
                       first_date = today() - years(10),
                       last_date = today(),
                       freq_data = "monthly")

usethis::use_data(monthly_stock_data, overwrite = TRUE)
