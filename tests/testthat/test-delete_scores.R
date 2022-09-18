test_that("delete_scores works", {
  scores <- tibble::tibble(x = 1:10)
  delete_scores(scores, NULL) %>%
    expect_equal(scores)
  delete_scores(scores, c(2,3)) %>%
    expect_equal(tibble::tibble(x = c(1, 4:10)))
  delete_scores(scores, c(2,11)) %>%
    expect_equal(scores)
  delete_scores(scores, c(11,12)) %>%
    expect_equal(scores)
  delete_scores(scores, 1:10) %>%
    expect_equal(tibble::tibble(x = numeric(), .rows = 0))
  delete_scores(scores, 10) %>%
    expect_equal(tibble::tibble(x = 1:9))
})
