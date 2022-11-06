test_that("score_distributions works", {
  withr::local_seed(42)
  
  score_distributions(NULL) %>%
    expect_null()
  
  scores <- tibble::tibble(
    x = 1:10,
    y = 10:1
  )
  score_distributions(scores) %>%
    vdiffr::expect_doppelganger(title = "Score distributions")
  
  scores2 <- tibble::tibble(
    x = 1:10,
    y = 5:14,
    z = NA
  )
  plot <- score_distributions(scores2)
  suppressWarnings(
    vdiffr::expect_doppelganger("Score distributions with missing score", plot)
  )
})

test_that("score_performance works", {
  df <- tibble::tibble(x = 1:10, s1 = 1:10)
  scores <- tibble::tibble(
    s1 = 1:10,
    s2 = 10:1
  )
  
  score_performance(df, NULL, scores) %>%
    expect_null()
  score_performance(df, "z", scores) %>%
    expect_null()
  score_performance(df, "s1", scores) %>%
    expect_null()
  score_performance(tibble::add_column(df, z = NA), 
                    "z", scores) %>%
    expect_null()
  score_performance(df, "x", scores) %>%
    vdiffr::expect_doppelganger(title = "Score performance")
  
  scores2 <- tibble::tibble(
    s1 = 1:10,
    s2 = NA
  )
  plot <- score_performance(df, "x", scores2)
  suppressWarnings(
    vdiffr::expect_doppelganger("Score performance with missing values", plot)
  )
})
