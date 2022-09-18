test_that("validate_plotting_method works", {
  validate_plotting_method(NULL) %>%
    expect_null()
  validate_plotting_method("aa") %>%
    expect_null()
  validate_plotting_method("line") %>%
    ggplot2::is.ggproto() %>%
    expect_true()
})

test_that("validate_plot_args works", {
  df <- tibble::tibble(
    x = 1:10,
    y = "a"
  )
  validate_plot_args(list(x = "x", y = "x", colour = NULL), df) %>%
    expect_equal(list(x = "x", y = "x"))
  validate_plot_args(list(x = "x", y = "x", colour = "z"), df) %>%
    expect_equal(list(x = "x", y = "x"))
  validate_plot_args(list(x = NULL, y = "z"), df) %>%
    expect_null()
})

test_that("custom_plot works", {
  df <- tibble::tibble(x = c(1,2,3,4,5,6), y = letters[1:6])
  custom_plot(df, NULL) %>%
    expect_null()
  custom_plot(df, "aaa") %>%
    expect_null()
  custom_plot(df, "line", x = "z", y = "a") %>%
    expect_null()
  custom_plot(df, "line", x = "x", y = "z") %>%
    expect_null()
  custom_plot(df, "line", x = "x", y = "x", colour = "x") %>%
    expect_snapshot_output()
  custom_plot(df, "scatter", x = "x", y = "y", colour = "y", size = "x", shape = "y") %>%
    expect_snapshot_output()
  plot <- custom_plot(df, "histogram", x = "x", colour = "y")
  expect_snapshot_output(plot)
  custom_plot(df, "histogram", x = "x", colour = "y", size = "z") %>%
    expect_equal(plot)
})
