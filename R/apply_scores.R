#' @export
apply_scores <- function(df, scores){
  if(nrow(scores) == 0){
    return(df)
  }
  purrr::pmap(scores, ~ score_column(df[[..2]], ...)) %>%
    rlang::set_names(scores$score_name) %>%
    dplyr::bind_cols(df, .)
}

score_column <- function(x, score_type, lb, ub, centre, inverse, exponential,
                         logarithmic, magnitude, custom_args, ...){
  tidyr::replace_na(score_type, "Linear") %>%
    switch(
      Peak = score_peak(x, lb, ub, centre, inverse),
      "Custom coordinates" = score_custom(x, custom_args),
      score_linear(x, lb, ub)
    ) %>%
    transform_exponential(exponential, logarithmic, magnitude)
}

score_linear <- function(x, lb, ub){
  if(is.null(x)){
    return(NA)
  } else if(lb <= ub){
    dplyr::case_when(
      x <= lb ~ 0,
      x >= ub ~ 1,
      x >= lb & x <= ub ~ (x-lb)/(ub-lb)
    )
  } else{
    dplyr::case_when(
      x <= ub ~ 1,
      x >= lb ~ 0,
      x >= ub & x <= lb ~ (lb-x)/(lb-ub)
    )
  }
}

score_peak <- function(x, lb, ub, centre, inverse){
  if(is.null(x)){
    return(NA)
  } else if(!inverse){
    dplyr::case_when(
      x <= lb | x >= ub ~ 0,
      x <= centre ~ (x-lb)/(centre-lb),
      x > centre ~ (ub-x)/(ub-centre)
    )
  } else{
    dplyr::case_when(
      x <= lb | x >= ub ~ 1,
      x <= centre ~ (centre-x)/(centre-lb),
      x > centre ~ (x-centre)/(ub-centre)
    )
  }
}

score_custom <- function(x, coords){
  if(is.null(x)){
    return(NA)
  }
  format_coords(coords) %>%
    purrr::pmap(score_coord_section, column = x) %>%
    combine_score_sections()

}

format_coords <- function(df){
  df_head <- head(df, -1)
  df_tail <- tail(df, -1) %>%
    rlang::set_names("next_x", "next_y")
  dplyr::bind_cols(df_head, df_tail)
}

score_coord_section <- function(column, x, y, next_x, next_y){
  dplyr::case_when(
    column < x | column > next_x ~ NA_real_,
    x == next_x ~ next_y,
    y <= next_y ~ (column-x)/(next_x-x) * (next_y-y) + y,
    y > next_y ~ (next_x-column)/(next_x-x) * (y-next_y) + next_y,
  )
}

combine_score_sections <- function(x){
  x[length(x):1] %>%
    dplyr::coalesce(!!!.)
}

transform_exponential <- function(x, exponential, logarithmic, magnitude){
  if(is.null(x)){
    return(NA)
  } else if(!exponential || magnitude == 0){
    return(x)
  } else if(logarithmic){
    magnitude <- -magnitude
  }
  base <- 10^magnitude
  dplyr::case_when(
    x == 0 ~ 0,
    x == 1 ~ 1,
    base == 0 ~ 1,
    is.infinite(base) ~ 0,
    T ~ (base^x - 1)/(base-1)
  )
}
