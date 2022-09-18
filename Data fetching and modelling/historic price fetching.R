library(yfR)
library(tidyverse)
library(rlang)
library(lubridate)

if(!is_interactive()){
  abort("This must be run in an interactive session")
}

sp500 <- yf_index_composition("SP500")
symbols <- sp500$ticker

monthly_data <- yf_get(symbols,
                       first_date = today() - years(10),
                       last_date = today(),
                       freq_data = "monthly")

daily_data <- yf_get(symbols,
                     first_date = today() - months(6),
                     last_date = today(),
                     freq_data = "daily")

use_data_raw("daily_stock_data")
