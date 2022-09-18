predict_price <- function(model, stock, start_date = lubridate::today(),
                          end_date = lubridate::today() + lubridate::years(4),
                          freq = c("daily", "monthly")){
  freq <- rlang::arg_match(freq)
  if(is.null(stock) || is.null(start_date) || is.null(end_date) || !stock %in% df$symbol ||
     start_date > end_date){
    return(NULL)
  }
  tibble::tibble(ticker = stock,
                 date = seq(start_date, end_date, by =
                              switch(freq,
                                     daily = "day",
                                     monthly = "month"
                              ))
  ) %>%
    augment(x = model)
}

plot_predictions <- function(predicted){
  if(is.null(predicted)){
    return(NULL)
  }
  ggplot2::ggplot(predicted, ggplot::aes(x = date, y = .pred)) +
    ggplot2::geom_line()
}

