test_that("get_scores works", {
  scores <- tibble::tibble(
    score_name = c("aaaa", "bbbb")
  )
  get_scores(tibble::tibble(x = 1, y = 2), scores = tibble::tibble()) %>%
    expect_null()
  get_scores(tibble::tibble(
    x = c(1, 2),
    y = c(3, 4),
    z = c(5, 6)
  ), scores = scores, final_score = F) %>%
    expect_equal(tibble::tibble(
      y = c(3, 4),
      z = c(5, 6)
    ))
  get_scores(tibble::tibble(
    x = c(1, 2),
    y = c(3, 4),
    z = c(5, 6),
    a = c(7, 8)
  ), scores = scores, final_score = T) %>%
    expect_equal(tibble::tibble(
      y = c(3, 4),
      z = c(5, 6),
      a = c(7, 8)
    ))
})
