#' Calculate a final score
#'
#' Add a 'final_score' column to a data frame that has been scored using
#' [apply_scores()] by calculating a weighted mean of all other scores.
#'
#' @param df A data frame. The result of [apply_scores()].
#' @param scores scores A data frame of score specifications. See [scores_init].
#'   This must be the same as the scores passed into [apply_scores()].
#'
#' @details
#' If no scores were created, the final score will not be added.]
#'
#' @seealso
#' * [scores_init] for the format of the `scores` table.
#' * [apply_scores()] for the prior step.
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
#' score_final(scored, scores)
#'
#' @export
score_final <- function(df, scores) {
  if (nrow(scores) == 0) {
    return(df)
  }
  actual_scores <- get_scores(df, scores, final_score = F)
  
  # Calculate a weighted mean of all the other scores
  purrr::pmap_dbl(actual_scores, ~ {
    stats::weighted.mean(c(...), w = scores$weight, na.rm = T)
  }) %>%
    dplyr::bind_cols(df, final_score = .)
}
