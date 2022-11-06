#' @export
fix_names <- function(df, scores) {
  new_names <- vctrs::vec_as_names(c(scores$score_name, colnames(df)), quiet = T)
}
