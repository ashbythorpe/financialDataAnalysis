#' Create a set of scores
#'
#' Apply a table of score specification, where each specification is represented
#' as a row, to a data frame. The set of created scores are then added back to
#' the original data frame as columns.
#'
#' @param df A data frame to score.
#' @param scores A data frame of score specifications. See [scores_init].
#'
#' @returns `df`, with scores added as columns.
#'
#' @seealso
#' * [scores_init] for the format of the `scores` table.
#' * [create_score()] and [delete_scores()] to create, edit and delete score
#'   specifications.
#' * [score_final()] to create a final score after these scores have been
#'   created.
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
#'
#' apply_scores(data, scores)
#'
#' @export
apply_scores <- function(df, scores) {
  if (nrow(scores) == 0) {
    return(df)
  }
  
  # Create scores, then add them to df
  purrr::pmap(scores, ~ score_column(df[[..2]], ...)) %>%
    rlang::set_names(scores$score_name) %>% # Set column names to score names
    dplyr::bind_cols(df, .)
}

score_column <- function(x, score_type, lb, ub, centre, inverse, exponential,
                         logarithmic, magnitude, custom_args, ...) {
  if (is.null(x)) {
    return(NA)
  }
  switch(score_type,
    Peak = score_peak(x, lb, ub, centre, inverse),
    "Custom coordinates" = score_custom(x, custom_args),
    score_linear(x, lb, ub)
  ) %>%
    transform_exponential(exponential, logarithmic, magnitude)
}

score_linear <- function(x, lb, ub) {
  if (lb <= ub) {
    dplyr::case_when(
      x <= lb ~ 0,
      x >= ub ~ 1,
      x >= lb & x <= ub ~ (x - lb) / (ub - lb)
    )
  } else {
    # lb and ub switch roles
    dplyr::case_when(
      x <= ub ~ 1,
      x >= lb ~ 0,
      x >= ub & x <= lb ~ (lb - x) / (lb - ub)
    )
  }
}

score_peak <- function(x, lb, ub, centre, inverse) {
  if (!inverse) {
    dplyr::case_when(
      x <= lb | x >= ub ~ 0,
      x <= centre ~ (x - lb) / (centre - lb),
      x > centre ~ (ub - x) / (ub - centre)
    )
  } else {
    # Equivalent to 1 - the last method
    dplyr::case_when(
      x <= lb | x >= ub ~ 1,
      x <= centre ~ (centre - x) / (centre - lb),
      x > centre ~ (x - centre) / (ub - centre)
    )
  }
}

score_custom <- function(x, coords) {
  format_coords(coords) %>% # Turn into a set of score sections
    purrr::pmap(score_coord_section, column = x) %>%
    combine_score_sections()
}

format_coords <- function(df) {
  df_head <- head(df, -1) # All except the last row
  df_tail <- tail(df, -1) %>% # All except the first row
    rlang::set_names("next_x", "next_y")
  dplyr::bind_cols(df_head, df_tail)
}

score_coord_section <- function(column, x, y, next_x, next_y) {
  dplyr::case_when(
    # Only concerned with values within the range of the coord section
    column < x | column > next_x ~ NA_real_,
    x == next_x ~ as.double(next_y),
    y <= next_y ~ as.double((column - x) / (next_x - x) * (next_y - y) + y),
    y > next_y ~ as.double((next_x - column) / (next_x - x) * (y - next_y) + next_y),
  )
}

combine_score_sections <- function(x) {
  rev(x) %>% # Later value is prioritized in matches
    dplyr::coalesce(!!!.)
}

transform_exponential <- function(x, exponential, logarithmic, magnitude) {
  if (!exponential || magnitude == 0) {
    return(x)
  } else if (logarithmic) {
    magnitude <- -magnitude
  }
  base <- 10^magnitude
  dplyr::case_when(
    x == 0 ~ 0,
    x == 1 ~ 1,
    base == 0 ~ 1,
    is.infinite(base) ~ 0,
    TRUE ~ (base^x - 1) / (base - 1) # Will run in the event of no other case
  )
}
