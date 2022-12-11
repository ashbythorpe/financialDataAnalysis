#' Create or edit a score specification
#'
#' Add to a table of score specifications, where each one is represented by a
#' row. The score specification will only be added if all arguments are valid.
#' Alternatively, edit an existing score specification with the `editing`
#' parameter.
#'
#' @param scores A data frame of score specifications. See [scores_init].
#' @param editing The row number of the score specification to edit. If NA (the
#'   default), add the newly created score to the end of `scores`.
#' @param score_type The method to use when creating the score. Either "Linear",
#'   "Peak" or "Custom coordinates".
#' @param colname The name of the column to score.
#' @param score_name The resulting name of the score. If "Default", the score
#'   will be given a sensible and informative default name.
#' @param weight The weight of the score when calculating the final score. See
#'   [score_final()].
#' @param lb For a linear or peak score, the lower bound of the score. This will
#'   produce a score of 0 in most cases.
#' @param ub For a linear or peak score, the upper bound of the score. This will
#'   usually produce a score of 1 for linear scores and a score of 0 for peak
#'   scores.
#' @param centre For a peak score, the centre of the score. This will produce
#'   a score of 1 unless inverse is `TRUE`.
#' @param inverse For a peak score, whether to invert the scoring method.
#' @param exponential Whether to apply an exponential transformation to the
#'   resulting score.
#' @param logarithmic If `exponential` is `TRUE`, whether to invert the
#'   exponential transformation.
#' @param magnitude If `exponential` is `TRUE`, the numeric magnitude of the
#'   exponential transformation.
#' @param custom_args For a custom score, a data frame of coordinates used to
#'   create the score.
#'
#' @details
#' # Linear scores
#' When the `score_name` is "Linear", a linear score is created. To make the
#' score you need to specify the `lb` and `ub` arguments.
#'
#' If the column value is less than or equal to the `lb` argument, the score is
#' 0.
#' If the column value is more than or equal to the `ub` argument, the score is
#' 1.
#'
#' Otherwise, the score is defined is the proportion of the distance of the
#' column value between the `lb` and `ub`.
#'
#' If the `lb` argument is more than the `ub` argument, the score is inverted.
#' This means that the `lb` produces a score of 1, the `ub` produces a score of
#' 0, etc.
#'
#' # Peak scores
#' When the `score_name` is "Peak", a peak score is created. To make the score
#' you need to specify the `lb`, `ub`, `centre` and `inverse` arguments. The
#' `lb`, `ub` and `centre` arguments must be numeric, and the `centre` must be
#' between the `lb` and `ub`.
#'
#' If the column value is less than or equal to the `lb` argument, the score is
#' 0.
#' If the column value is equal to the `centre` argument, the score is 1.
#' If the column value is more than or equal to the `ub` argument, the score is
#' 1.
#'
#' If the column value is in between the `lb` and `centre` arguments, the score
#' is defined as the proportion of the column value along between the `lb` and
#' `centre`.
#' If the column value is in between the `centre` and `ub` arguments, the score
#' is defined as the proportion of the column value along between the `ub` and
#' `centre`.
#'
#'
#' When `inverse` is `TRUE`, the score is inverted: the lower bound and upper
#' bound produce a score of 1, and the centre produces a score of 0.
#'
#'
#' # Custom scores
#' When `score_type` is "Custom coordinates", a custom score is created. This
#' allows you to define a set of coordinates, where the x coordinate is a value
#' in the column, and the y coordinate is a score between 0 and 1. The score
#' will then be created by connecting the coordinates together. The coordinates
#' should be in the form of a data frame, with the x coordinates in the 'x'
#' column and the y coordinates in the 'y' column.
#'
#' This can be used to create a huge variety of different scores.
#'
#' For more detailed information, along with plots that demonstrate the 3
#' scoring methods, see the Prototype 2 vignette:
#' `vignette("prototype2", package = "financialDataAnalysis")`
#'
#' @returns
#' If any required arguments are not valid, `scores` is returned as is.
#' If `editing` is `NA`, `scores` is returned with an ectra row.
#' If `editing` is a valid row number, `scores` with 1 edited row is returned.
#'
#' @seealso
#' * [scores_init] for the format of the `scores` table.
#' * [delete_scores()] for the inverse operation.
#' * [apply_scores()] for the creation of the actual scores.
#'
#' @examples
#' scores <- create_score(
#'   scores_init,
#'   score_type = "Linear", colname = "x", score_name = "Default",
#'   weight = 1, lb = 1, ub = 6, exponential = FALSE
#' )
#'
#' # Change the weight
#' create_score(
#'   scores,
#'   editing = 1, score_type = "Linear", colname = "x",
#'   score_name = "Default", weight = 2, lb = 1, ub = 6, exponential = FALSE
#' )
#'
#' @export
create_score <- function(scores,
                         editing = NA,
                         score_type = NULL,
                         colname = NULL,
                         score_name = NULL,
                         weight = NULL,
                         lb = NULL,
                         ub = NULL,
                         centre = NULL,
                         inverse = NULL,
                         exponential = NULL,
                         logarithmic = NULL,
                         magnitude = NULL,
                         custom_args = NULL) {
  row_to_add <- validate_score(
    score_type, colname, score_name, weight, lb, ub,
    centre, inverse, exponential, logarithmic,
    magnitude, custom_args
  )
  if (is.null(row_to_add)) {
    return(scores)
  }
  .f <- get_score_function(editing)
  .f(scores, row_to_add) %>%
    replace_score_names()
}

validate_score <- function(score_type, colname, score_name, weight, lb, ub,
                           centre, inverse, exponential, logarithmic,
                           magnitude, custom_args) {
  validated_score_type <- validate_score_type(score_type)
  universal_row <- validate_universal_score(colname, score_name, weight)
  exponential_row <- validate_exponential_transformation(exponential, logarithmic, magnitude)
  if (is.null(validated_score_type)) {
    return(NULL)
  } else if (validated_score_type == "Linear") {
    score_row <- validate_linear_score(lb, ub)
  } else if (validated_score_type == "Peak") {
    score_row <- validate_peak_score(lb, ub, centre, inverse)
  } else if (validated_score_type == "Custom coordinates") {
    score_row <- validate_custom_score(custom_args)
  }
  bind_validated_columns(
    validated_score_type, universal_row,
    score_row, exponential_row
  )
}

validate_score_type <- function(x) {
  opts <- c("Linear", "Peak", "Custom coordinates")
  if (is.character(x) && x %in% opts) {
    x
  } else {
    NULL
  }
}

validate_universal_score <- function(colname, score_name, weight) {
  colname_valid <- is.character(colname) && colname != ""
  score_name_valid <- is.character(score_name)
  weight_valid <- is.numeric(weight)
  if (colname_valid && score_name_valid && weight_valid) {
    tibble::tibble_row(
      colname = colname,
      score_name =
        if (stringr::str_to_lower(score_name) %in% c("", "default")) {
          NA_character_
        } else if (stringr::str_length(score_name) > 100) {
          stringr::str_sub(score_name, end = 100L)
        } else {
          score_name
        },
      weight = weight
    )
  } else {
    NULL
  }
}

validate_exponential_transformation <- function(exponential, logarithmic, magnitude) {
  if (!is.logical(exponential) ||
    (exponential &&
      (!is.logical(logarithmic) || !is.numeric(magnitude)))) {
    NULL
  } else if (!exponential) {
    tibble::tibble_row(exponential = exponential)
  } else {
    tibble::tibble_row(exponential = exponential, logarithmic = logarithmic, magnitude = magnitude)
  }
}

validate_linear_score <- function(lb, ub) {
  if (!is.numeric(lb) || !is.numeric(ub)) {
    NULL
  } else {
    tibble::tibble(lb = lb, ub = ub)
  }
}

validate_peak_score <- function(lb, ub, centre, inverse) {
  if (!is.numeric(lb) || !is.numeric(ub) || !is.logical(inverse)) {
    return(NULL)
  } else if (lb > ub) {
    # swap ub and lb
    ub <- lb + ub
    lb <- ub - lb
    ub <- ub - lb
  }
  if (!is.numeric(centre) || centre < lb || centre > ub) {
    NULL
  } else {
    tibble::tibble(lb = lb, ub = ub, centre = centre, inverse = inverse)
  }
}

validate_custom_score <- function(x) {
  if (!is.data.frame(x) || any(!colnames(x) %in% c("x", "y")) ||
    !all(c("x", "y") %in% colnames(x))) {
    return(NULL)
  }
  unique_df <- dplyr::filter(x, is.numeric(x) & !is.na(x) & is.numeric(y) & !is.na(y)) %>%
    dplyr::distinct()
  if (nrow(unique_df) < 2) {
    return(NULL)
  }
  arranged <- dplyr::arrange(unique_df, x)
  tibble::tibble(custom_args = list(arranged))
}

bind_validated_columns <- function(score_type, ...) {
  dfs <- rlang::list2(...)
  if (purrr::some(dfs, is.null)) {
    NULL
  } else {
    dplyr::bind_cols(score_type = score_type, ...)
  }
}

get_score_function <- function(editing) {
  if (is.na(editing)) {
    function(df, row) {
      tibble::add_row(df, row)
    }
  } else {
    function(df, row) {
      if (is.null(editing) || editing > nrow(df)) {
        return(df)
      }
      df %>%
        dplyr::slice(-editing) %>%
        tibble::add_row(row, .before = editing)
    }
  }
}

# version made for testing
edit_row <- function(df, row, editing) {
  if (is.null(editing) || editing > nrow(df)) {
    return(df)
  }
  df %>%
    dplyr::slice(-editing) %>%
    tibble::add_row(row, .before = editing)
}

replace_score_names <- function(df) {
  dplyr::mutate(
    df,
    score_name = dplyr::coalesce(
      score_name,
      glue::glue("Score {dplyr::row_number()}: {colname}")
    )
  )
}
