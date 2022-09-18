#' @export
score_final <- function(df, scores){
  if(nrow(scores) == 0){
    return(df)
  }
  actual_scores <- get_scores(df, scores, final_score = F)
  final <- purrr::pmap_dbl(actual_scores, ~ {
    stats::weighted.mean(c(...), w = scores$weight, na.rm = T)
  }) %>%
    dplyr::bind_cols(df, final_score = .)
}
