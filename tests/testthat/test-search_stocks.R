test_that("search_pattern works", {
  search_pattern(tibble::tibble(
    symbol = c("MMM", "AOS", "ABT"),
    companyName = c("3M Co.", "A.O. Smith Corp.", "Abbott Laboratories")
  ), pattern = "m") %>%
    expect_equal(c(T, T, F))
  search_pattern(tibble::tibble(
    symbol = c("MMM", "AOS", "ABT"),
    companyName = c("3M Co.", "A.O. Smith Corp.", "Abbott Laboratories")
  ), pattern = "AO") %>%
    expect_equal(c(F, T, F))
  search_pattern(tibble::tibble(
    symbol = c("MMM", "AOS", "ABT"),
    companyName = c("3M Co.", "A.O. Smith Corp.", "Abbott Laboratories")
  ), pattern = "") %>%
    expect_equal(c(T, T, T))
})

test_that("search_stocks works", {
  search_stocks(tibble::tibble(
    symbol = c("MMM", "AOS", "ABT"),
    companyName = c("3M Co.", "A.O. Smith Corp.", "Abbott Laboratories")
  ), pattern = "ao m") %>%
    expect_equal(tibble::tibble(
      symbol = "AOS",
      companyName = "A.O. Smith Corp."
    ))
  search_stocks(tibble::tibble(
    symbol = c("MMM", "AOS", "ABT"),
    companyName = c("3M Co.", "A.O. Smith Corp.", "Abbott Laboratories")
  ), pattern = "bt lab") %>%
    expect_equal(tibble::tibble(
      symbol = "ABT",
      companyName = "Abbott Laboratories"
    ))
  search_stocks(tibble::tibble(
    symbol = c("MMM", "AOS", "ABT"),
    companyName = c("3M Co.", "A.O. Smith Corp.", "Abbott Laboratories")
  ), pattern = "   ") %>%
    expect_equal(tibble::tibble(
      symbol = c("MMM", "AOS", "ABT"),
      companyName = c("3M Co.", "A.O. Smith Corp.", "Abbott Laboratories")
    ))
  search_stocks(tibble::tibble(
    symbol = c("MMM", "AOS", "ABT"),
    companyName = c("3M Co.", "A.O. Smith Corp.", "Abbott Laboratories")
  ), pattern = "AOSA") %>%
    expect_null()
})

test_that("search_results works", {
  search_results(NULL) %>%
    expect_null()
  search_results(tibble::tibble(
    symbol = c("MMM", "AOS", "ABT"),
    companyName = c("3M Co.", "A.O. Smith Corp.", "Abbott Laboratories"),
    favourite = c(F, T, T)
  )) %>%
    expect_equal(tibble::tibble(
      symbol = c("AOS", "ABT", "MMM"),
      companyName = c("A.O. Smith Corp.", "Abbott Laboratories", "3M Co.")
    ))
})
