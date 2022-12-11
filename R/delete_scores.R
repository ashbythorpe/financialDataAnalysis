#' Remove one or more score specifications
#'
#' Delete one or more rows from a scores table, where each row represents a
#' score specification.
#'
#' @param scores A data frame of score specifications. See [scores_init].
#' @param rows A numeric vector of rows to remove from the table.
#'
#' @returns `scores` with some rows removed.
#'
#' @seealso
#' * [scores_init] for the format of the `scores` table.
#' * [create_score()] for the inverse operation.
#' * [apply_scores()] for the creation of the actual scores.
#'
#' @examples
#' scores <- create_score(
#'   scores_init,
#'   score_type = "Linear", colname = "x", score_name = "Default",
#'   weight = 1, lb = 1, ub = 6, exponential = FALSE
#' )
#'
#' delete_scores(scores, 1)
#'
#' @export
delete_scores <- function(scores, rows) {
  if (is.null(rows) || max(rows) > nrow(scores)) {
    return(scores)
  } else {
    dplyr::slice(scores, -rows)
  }
}
