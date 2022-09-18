get_scores <- function(df, scores, final_score = T){
  if(nrow(scores) == 0){
    return(NULL)
  }
  if(final_score){
    n_scores <- nrow(scores) + 1
  } else{
    n_scores <- nrow(scores)
  }
  dplyr::select(df, (length(df)-n_scores+1):length(df))
}
