## code to prepare `favicon` dataset goes here

data <- purrr::map(1:10, ~ {
  tibble::tibble(
    x = runif(20),
    y = 1.1 - x
  )
}) %>%
  dplyr::bind_cols() %>%
  dplyr::mutate(base = 0) %>%
  dplyr::relocate(base) %>%
  tidyr::pivot_longer(dplyr::everything())

data$x <- rep(1:20, each = 21)

data <- tibble::add_row(data, data[data$x == 1,] %>%
                          dplyr::mutate(x = 21))

ggplot2::ggplot(data, ggplot2::aes(x = x, y = value, fill = name)) +
  ggplot2::geom_area() +
  ggplot2::scale_fill_viridis_d(guide = "none") +
  ggplot2::coord_polar() +
  ggplot2::theme_void()
