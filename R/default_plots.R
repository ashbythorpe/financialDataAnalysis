#' @export
score_distributions <- function(scores){
  if(is.null(scores)){
    return(NULL)
  }
  
  tidyr::pivot_longer(scores, tidyselect::everything()) %>%
    ggplot2::ggplot(ggplot2::aes(x = "", y = value)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter() +
    ggplot2::facet_grid(.~name) +
    ggplot2::scale_x_discrete(breaks = NULL, position = "top") +
    ggplot2::labs(x = "Score name", y = "Score")
}

#' @export
score_performance <- function(df, colname, scores){
  if(is.null(colname) || !colname %in% colnames(df) || 
     colname %in% colnames(scores) || all(is.na(df[[colname]]))){
    return(NULL)
  }
  
  scores %>%
    dplyr::bind_cols(!!colname := df[[colname]]) %>%
    tidyr::pivot_longer(-tidyselect::any_of(colname)) %>%
    ggplot2::ggplot(ggplot2::aes(x = value, y = .data[[colname]], 
                                 color = name)) +
    ggplot2::geom_line()
}
