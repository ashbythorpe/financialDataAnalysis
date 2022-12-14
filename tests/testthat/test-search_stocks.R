test_that("search_pattern works", {
  search_pattern(tibble::tibble(
    symbol = c("MMM", "AOS", "ABT"),
    company_name = c("3M Co.", "A.O. Smith Corp.", "Abbott Laboratories")
  ), pattern = "m") %>%
    expect_equal(c(T, T, F))
  search_pattern(tibble::tibble(
    symbol = c("MMM", "AOS", "ABT"),
    company_name = c("3M Co.", "A.O. Smith Corp.", "Abbott Laboratories")
  ), pattern = "AO") %>%
    expect_equal(c(F, T, F))
  search_pattern(tibble::tibble(
    symbol = c("MMM", "AOS", "ABT"),
    company_name = c("3M Co.", "A.O. Smith Corp.", "Abbott Laboratories")
  ), pattern = "") %>%
    expect_equal(TRUE)
})

test_that("search_stocks works", {
  search_stocks(tibble::tibble(
    symbol = c("MMM", "AOS", "ABT"),
    company_name = c("3M Co.", "A.O. Smith Corp.", "Abbott Laboratories")
  ), pattern = "ao m") %>%
    expect_equal(tibble::tibble(
      symbol = "AOS",
      company_name = "A.O. Smith Corp."
    ))
  search_stocks(tibble::tibble(
    symbol = c("MMM", "AOS", "ABT"),
    company_name = c("3M Co.", "A.O. Smith Corp.", "Abbott Laboratories")
  ), pattern = "bt lab") %>%
    expect_equal(tibble::tibble(
      symbol = "ABT",
      company_name = "Abbott Laboratories"
    ))
  search_stocks(tibble::tibble(
    symbol = c("MMM", "AOS", "ABT"),
    company_name = c("3M Co.", "A.O. Smith Corp.", "Abbott Laboratories")
  ), pattern = "   ") %>%
    expect_equal(tibble::tibble(
      symbol = c("MMM", "AOS", "ABT"),
      company_name = c("3M Co.", "A.O. Smith Corp.", "Abbott Laboratories")
    ))
  search_stocks(tibble::tibble(
    symbol = c("MMM", "AOS", "ABT"),
    company_name = c("3M Co.", "A.O. Smith Corp.", "Abbott Laboratories")
  ), pattern = "AOSA") %>%
    expect_null()
})

test_that("search_results works", {
  search_results(NULL) %>%
    expect_null()
  search_results(tibble::tibble(
    symbol = c("MMM", "AOS", "ABT"),
    company_name = c("3M Co.", "A.O. Smith Corp.", "Abbott Laboratories"),
    favourite = c(F, T, T)
  )) %>%
    expect_equal(tibble::tibble(
      symbol = c("AOS", "ABT", "MMM"),
      company_name = c("A.O. Smith Corp.", "Abbott Laboratories", "3M Co.")
    ))
})
