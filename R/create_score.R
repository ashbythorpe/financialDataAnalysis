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
                         custom_args = NULL){
  row_to_add <- validate_score(score_type, colname, score_name, weight, lb, ub,
                               centre, inverse, exponential, logarithmic,
                               magnitude, custom_args)
  if(is.null(row_to_add)){
    return(scores)
  }
  .f <- get_score_function(editing)
  .f(scores, row_to_add) %>%
    replace_score_names()
}

validate_score <- function(score_type, colname, score_name, weight, lb, ub,
                           centre, inverse, exponential, logarithmic,
                           magnitude, custom_args){
  validated_score_type <- validate_score_type(score_type)
  universal_row = validate_universal_score(colname, score_name, weight)
  exponential_row = validate_exponential_transformation(exponential, logarithmic, magnitude)
  if(is.null(validated_score_type)){
    return(NULL)
  } else if(validated_score_type == "Linear"){
    score_row = validate_linear_score(lb, ub)
  } else if(validated_score_type == "Peak"){
    score_row = validate_peak_score(lb, ub, centre, inverse)
  } else if(validated_score_type == "Custom coordinates"){
    score_row = validate_custom_score(custom_args)
  }
  bind_validated_columns(validated_score_type, universal_row,
                         score_row, exponential_row)
}

validate_score_type <- function(x){
  opts <- c("Linear", "Peak", "Custom coordinates")
  if(is.character(x) && x %in% opts){
    x
  } else{
    NULL
  }
}

validate_universal_score <- function(colname, score_name, weight){
  colname_valid <- is.character(colname) && colname != ""
  score_name_valid <- is.character(score_name)
  weight_valid <- is.numeric(weight)
  if(colname_valid && score_name_valid && weight_valid){
    tibble::tibble_row(
      colname = colname,
      score_name = dplyr::if_else(
        stringr::str_to_lower(score_name) %in% c("", "default"), 
        NA_character_, score_name
      ),
      weight = weight)
  } else{
    NULL
  }
}

validate_exponential_transformation <- function(exponential, logarithmic, magnitude){
  if(!is.logical(exponential) ||
     (exponential &&
      (!is.logical(logarithmic) || !is.numeric(magnitude)))){
    NULL
  } else if(!exponential){
    tibble::tibble_row(exponential = exponential)
  } else{
    tibble::tibble_row(exponential = exponential, logarithmic = logarithmic, magnitude = magnitude)
  }
}

validate_linear_score <- function(lb, ub){
  if(!is.numeric(lb) || !is.numeric(ub)){
    NULL
  } else{
    tibble::tibble(lb = lb, ub = ub)
  }
}

validate_peak_score <- function(lb, ub, centre, inverse){
  if(!is.numeric(lb) || !is.numeric(ub) || !is.logical(inverse)){
    return(NULL)
  } else if(lb > ub){
    # swap ub and lb
    ub <- lb + ub
    lb <- ub - lb
    ub <- ub - lb
  }
  if(!is.numeric(centre) || centre < lb || centre > ub){
    NULL
  } else{
    tibble::tibble(lb = lb, ub = ub, centre = centre, inverse = inverse)
  }
}

validate_custom_score <- function(x){
  if(!is.data.frame(x) || any(!colnames(x) %in% c("x", "y"))){
    return(NULL)
  }
  unique_df <- dplyr::filter(x, is.numeric(x) & !is.na(x) & is.numeric(y) & !is.na(y)) %>%
    dplyr::distinct()
  if(nrow(unique_df) < 2){
    return(NULL)
  }
  dplyr::arrange(unique_df, x) %>%
    tibble::tibble(custom_args = .)
}

bind_validated_columns <- function(score_type, ...){
  dfs = rlang::list2(...)
  if(purrr::some(dfs, is.null)){
    NULL
  } else{
    dplyr::bind_cols(score_type = score_type, ...)
  }
}

get_score_function <- function(editing){
  if(is.na(editing)){
    function(df, row){
      tibble::add_row(df, row)
    }
  } else{
    function(df, row){
      if(is.null(editing) || editing > nrow(df)){
        return(df)
      }
      df %>%
        dplyr::slice(-editing) %>%
        tibble::add_row(row, .before = editing)
    }
  }
}

#version made for testing
edit_row <- function(df, row, editing){
  if(is.null(editing) || editing > nrow(df)){
    return(df)
  }
  df %>%
    dplyr::slice(-editing) %>%
    tibble::add_row(row, .before = editing)
}

replace_score_names <- function(df){
  dplyr::mutate(
    df,
    score_name = dplyr::coalesce(
      score_name,
      glue::glue("Score {dplyr::row_number()}: {colname}")
    ))
}
