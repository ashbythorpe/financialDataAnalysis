test_that("custom_plot works", {
  skip_on_ci()
  df <- tibble::tibble(x = c(1,2,3,4,5,6), y = rep(letters[1:3], 2))
  custom_plot(df, NULL) %>%
    expect_null()
  custom_plot(df, "aaa") %>%
    expect_null()
  custom_plot(df, "line", x = "z", y = "a") %>%
    expect_null()
  custom_plot(df, "line", x = "x", y = "z") %>%
    expect_null()
  custom_plot(df, "line", x = "x", y = "x", colour = "x") %>%
    vdiffr::expect_doppelganger(title = "Custom line plot")
  custom_plot(df, "scatter", x = "x", y = "y", colour = "y", size = "x", 
              shape = "y") %>%
    vdiffr::expect_doppelganger(title = "Custom scatter plot")
  plot <- custom_plot(df, "histogram", x = "x", y = "y")
  plot2 <- custom_plot(df, "histogram", x = "x", y = "y", size = "z")
  expect_equal(plot, plot2)
  
  # Evaluating the plot changes it, so this must be after the expect_equal call
  vdiffr::expect_doppelganger("Custom histogram plot", plot)
  
  custom_plot(df, "line", x = "x", y = "x", opacity = 0.5) %>%
    vdiffr::expect_doppelganger(title = "Half opacity")
  custom_plot(df, "line", x = "x", y = "x", size = 10) %>%
    vdiffr::expect_doppelganger(title = "Large line width")
  custom_plot(df, "scatter", x = "x", y = "x", size = 10) %>%
    vdiffr::expect_doppelganger(title = "Large point size")
  custom_plot(df, "scatter", x = "x", y = "y", jitter = TRUE) %>%
    vdiffr::expect_doppelganger(title = "Jitter")
  custom_plot(df, "histogram", y = "x", bins = 10) %>%
    vdiffr::expect_doppelganger(title = "Custom histogram")
  custom_plot(df, "smooth", x = "x", y = "x", span = 10) %>%
    vdiffr::expect_doppelganger(title = "Smoothed plot")
  custom_plot(df, "area", x = "x", y = "x", fill = "y", colour = "y", 
              opacity = 0.75) %>%
    vdiffr::expect_doppelganger(title = "Area plot")
  custom_plot(df, "hex", x = "x", y = "x", fill = "y", bins = 10) %>%
    vdiffr::expect_doppelganger(title = "Hex plot")
  custom_plot(df, "line", x = "x", y = "x", alpha = "x")
})

test_that("validate_plotting_method works", {
  validate_plotting_method(NULL) %>%
    expect_null()
  validate_plotting_method("aa") %>%
    expect_null()
  validate_plotting_method("line") %>%
    expect_equal("line")
})

test_that("validate_plot_args works", {
  df <- tibble::tibble(
    x = 1:10,
    y = "a"
  )
  validate_plot_args(list(x = "x", y = "x", colour = NULL), df) %>%
    expect_equal(c(x = "x", y = "x"))
  validate_plot_args(list(x = "x", y = "x", colour = "z"), df) %>%
    expect_equal(c(x = "x", y = "x"))
  validate_plot_args(list(x = NULL, y = "z"), df) %>%
    expect_null()
})

test_that("subset_plot_args works", {
  subset_plot_args(NULL, "line") %>%
    expect_null()
  subset_plot_args(c(), "line") %>%
    expect_null()
  subset_plot_args(c(z = 1), "line") %>%
    expect_null()
  subset_plot_args(c(colour = 1), "scatter") %>%
    expect_null()
  subset_plot_args(c(x = 1), "scatter") %>%
    expect_null()
  subset_plot_args(c(x = 1, y = 2, size = 1), "histogram") %>%
    expect_equal(c(x = 1, size = 1))
  subset_plot_args(c(x = 1, y = 2, size = 1), "scatter") %>%
    expect_equal(c(x = 1, y = 2, size = 1))
})

test_that("create_plot works", {
  skip_on_ci()
  df <- tibble::tibble(x = 1:10, y = 10:1)
  create_plot(df, args = c(x = "x", y = "y"), "line") %>%
    vdiffr::expect_doppelganger(title = "Create_plot line graph")
  create_plot(df, args = c(x = "x", y = "y"), "scatter") %>%
    vdiffr::expect_doppelganger(title = "Create_plot scatter graph")
  create_plot(df, args = c(x = "x"), "histogram") %>%
    vdiffr::expect_doppelganger(title = "Create_plot histogram")
})

test_that("custom_plot works", {
  skip_on_ci()
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
    vdiffr::expect_doppelganger(title = "Custom line plot")
  custom_plot(df, "scatter", x = "x", y = "y", colour = "y", size = "x", 
              shape = "y") %>%
    vdiffr::expect_doppelganger(title = "Custom scatter plot")
  plot <- custom_plot(df, "histogram", x = "x", y = "y")
  plot2 <- custom_plot(df, "histogram", x = "x", y = "y", size = "z")
  expect_equal(plot, plot2)
  
  # Evaluating the plot changes it, so this must be after the expect_equal call
  vdiffr::expect_doppelganger("Custom histogram plot", plot)
})

test_that("print_plot works", {
  df <- tibble::tibble(x = 1:100, y = 100:1, z = letters[1:100])
  
  plot1 <- custom_plot(df, "scatter", x = "x", y = "y", shape = "x")
  expect_error(print(plot1))
  print_plot(plot1)
  
  plot2 <- custom_plot(df, "scatter", x = "x", y = "y", shape = "z")
  expect_warning(expect_warning(print(plot2))) # Two warnings are thrown
  print_plot(plot2)
  
  plot3 <- custom_plot(df, "line", x = "x", y = "z")
  print_plot(plot3)
})
