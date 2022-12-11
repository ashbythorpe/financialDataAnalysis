test_that("score_final works", {
  score_final(
    tibble::tibble(x = c(1, 2, 3), y = c(2, 3, 4), z = c(3, 4, 5)),
    tibble::tibble()
  ) %>%
    expect_equal(tibble::tibble(x = c(1, 2, 3), y = c(2, 3, 4), z = c(3, 4, 5)))
  score_final(
    tibble::tibble(
      unused_col = "a",
      x = c(1, 2, 3, 4, 5, 6),
      y = c(2, 3, 4, NA, 6, 10),
      z = c(3, 4, 5, NA, NA, 100)
    ),
    scores = tibble::tibble(weight = c(1, 2, 3))
  ) %>%
    expect_equal(tibble::tibble(
      unused_col = "a",
      x = c(1, 2, 3, 4, 5, 6),
      y = c(2, 3, 4, NA, 6, 10),
      z = c(3, 4, 5, NA, NA, 100),
      final_score =
        c(
          (1 * 1 + 2 * 2 + 3 * 3) / 6,
          (2 * 1 + 3 * 2 + 4 * 3) / 6,
          (3 * 1 + 4 * 2 + 5 * 3) / 6,
          4,
          (5 * 1 + 6 * 2) / 3,
          (6 * 1 + 10 * 2 + 100 * 3) / 6
        )
    ))
  score_final(tibble::tibble(
    final_score = "a",
    x = c(1, 2, 3)
  ), scores = tibble::tibble(weight = 1)) %>%
    expect_equal(tibble::tibble(
      final_score...1 = "a",
      x = c(1, 2, 3),
      final_score...3 = c(1, 2, 3)
    ))
})
