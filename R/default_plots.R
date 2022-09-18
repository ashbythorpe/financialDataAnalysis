score_distributions <- function(scores){
  if(is.null(scores)){
    return(NULL)
  }
  tidyr::pivot_longer(scores, tidyselect::everything(-date)) %>%
    ggplot2::ggplot(aes(x = date, y = value)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter() +
    ggplot2::facet_grid(.~name) +
    ggplot2::ylab("Score")
}

score_performance <- function(df, colname, scores){
  if(is.null(colname) || !colname %in% colnames(df) || colname %in% scores$score_name){
    return(NULL)
  }
  get_scores(df, scores) %>%
    dplyr::bind_cols(ticker = df$ticker) %>%
    ggplot2::ggplot(aes(x = value, y = .data[[colname]], color = name)) +
    ggplot2::geom_line()
}
