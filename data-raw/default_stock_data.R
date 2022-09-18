## code to prepare `default_stock_data` dataset goes here
library(Riex)
library(tidyverse)
library(lubridate)
library(rlang)
library(yfR)
library(furrr)

if(!rlang::is_interactive() || sk == ""){
  rlang::abort("This must be run in an interactive session with a valid `iex.cloud` secret key.")
}

sp500 <- yf_index_composition("SP500")

indexes <- sp500$ticker

company <- future_map(indexes,
                      possibly(iex.company, otherwise = NULL),
                      sk)

second_company <- map(indexes[map_lgl(company, is.null)],
                      possibly(iex.company, otherwise = NULL),
                      sk)

combined_company <- c(company, second_company) %>%
  compact()

cash_flow <- future_map(sp500$ticker,
                        possibly(iex.cash.flow, otherwise = NULL),
                        sk)

second_cash_flow <- map(sp500$ticker[map_lgl(cash_flow, is.null)],
                        possibly(iex.cash.flow, otherwise = NULL, quiet = F),
                        sk)

combined_cash_flow <- c(cash_flow, second_cash_flow) %>%
  compact()

balance_sheet <- future_map(sp500$ticker,
                            possibly(iex.balance.sheet, otherwise = NULL),
                            sk)

second_balance_sheet <- map(sp500$ticker[map_lgl(balance_sheet, is.null)],
                            possibly(iex.balance.sheet, otherwise = NULL),
                            sk)

combined_balance_sheet <- c(balance_sheet, second_balance_sheet) %>%
  compact()

income <- future_map(sp500$ticker,
                     possibly(iex.income, otherwise = NULL),
                     sk)

second_income <- map(sp500$ticker[map_lgl(income, is.null)],
                     possibly(iex.income, otherwise = NULL),
                     sk)

combined_income <- c(income, second_income) %>%
  compact()

stats <- future_map(sp500$ticker,
                    possibly(iex.stats, otherwise = NULL),
                    sk)

second_stats <- map(sp500$ticker[map_lgl(stats, is.null)],
                    possibly(iex.stats, otherwise = NULL),
                    sk)

combined_stats <- c(stats, second_stats) %>%
  compact()

final_company <- bind_rows(combined_company)
final_cash_flow <- bind_rows(combined_cash_flow)
final_balance_sheet <- bind_rows(combined_balance_sheet)
final_income <- bind_rows(combined_income)

tidy_cash_flow <-
  final_cash_flow %>%
  as_tibble() %>%
  select(-id) %>%
  rename(cash_flow_updated = "updated")

tidy_balance_sheet <-
  final_balance_sheet %>%
  as_tibble() %>%
  select(-id) %>%
  rename(balance_sheet_updated = "updated")

tidy_income <-
  final_income %>%
  as_tibble() %>%
  select(-id, -minorityInterest) %>%
  rename(income_updated = "updated")

pivot_stats <- function(x){
  x %>%
    as_tibble() %>%
    pivot_wider(names_from = "Field", values_from = "Value")
}

tidy_stats <- combined_stats %>%
  map(pivot_stats) %>%
  bind_rows() %>%
  select(-employees)

joined <-
  inner_join(final_company, tidy_cash_flow) %>%
  inner_join(tidy_balance_sheet) %>%
  inner_join(tidy_income) %>%
  inner_join(tidy_stats)

issueType_table <- tribble(
  ~issueType, ~`issue_type`,
  "ad", "ADR",
  "cs", "Common Stock",
  "cef", "Closed End Fund",
  "et", "ETF",
  "oef", "Open Ended Fund",
  "ps", "Preferred Stock",
  "rt", "Right",
  "struct", "Structured Product",
  "ut", "Unit",
  "wi", "When Issued",
  "wt", "Warrant"
)

chr <- map_lgl(joined, is.character)
num <- suppressWarnings(
  map_lgl(joined, ~ {mean(!is.na(as.numeric(.))) > 0.7})
)
should_be_numeric <- colnames(joined)[chr & num]

milliseconds_to_datetime <- function(x){
  x %>%
    as.numeric() %>%
    milliseconds() %>%
    as_datetime()
}

map_chr(final_data, vctrs::vec_ptype_show)
final_data
final_data$date

default_stock_data <- joined %>%
  mutate(across(all_of(should_be_numeric), as.numeric)) %>%
  mutate(across(c("fiscalDate", "reportDate"), ymd)) %>%
  mutate(across(where(is.character), ~ ifelse(. == "", NA_character_, .))) %>%
  left_join(issueType_table) %>%
  select(-issueType, -float,
         -fiscalQuarter, -fiscalYear) %>%
  mutate(across(c("cash_flow_updated", "balance_sheet_updated",
                  "income_updated", "date"),
                milliseconds_to_datetime)) %>%
  rename(EBIT = "ebit")

usethis::use_data(default_stock_data, overwrite = TRUE)
