#' @export
delete_scores <- function(scores, rows){
  if(is.null(rows) || max(rows) > nrow(scores)){
    return(scores)
  } else{
    dplyr::slice(scores, -rows)
  }
}
