#' Get the created scores from a scored data frame.
#'
#' Extract the score columns from a data frame scored using [apply_scores()].
#'
#' @param df The scored data frame.
#' @param scores A data frame of score specifications. See [scores_init].
#' @param final_score Whether the data frame has been given a final score by
#'   [score_final()].
#'
#' @returns A data frame of scores.
#'
#' @examples
#' data <- tibble::tibble(
#'   x = 1:10
#' )
#'
#' scores <- create_score(
#'   scores_init,
#'   score_type = "Linear", colname = "x", score_name = "Default",
#'   weight = 1, lb = 1, ub = 6, exponential = FALSE
#' )
#' scores <- create_score(
#'   scores,
#'   score_type = "Peak", colname = "x", score_name = "Peak score",
#'   weight = 2, lb = 2, ub = 8, centre = 5, inverse = FALSE,
#'   exponential = FALSE
#' )
#'
#' scored <- apply_scores(data, scores)
#' scored <- score_final(scored, scores)
#'
#' get_scores(scored, scores)
#'
#' @export
get_scores <- function(df, scores, final_score = TRUE) {
  if (nrow(scores) == 0) {
    return(NULL)
  }
  if (final_score) {
    n_scores <- nrow(scores) + 1
  } else {
    n_scores <- nrow(scores)
  }
  dplyr::select(df, (length(df) - n_scores + 1):length(df))
}
