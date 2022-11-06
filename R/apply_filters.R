#' @export
apply_filters <- function(df, filters){
  if(nrow(filters) == 0){
    return(df)
  }
  filtered <- purrr::pmap(filters, ~ filter_column(df[[..2]], ...)) %>%
    purrr::reduce(`&`, .init = T) %>%
    {dplyr::filter(df, .env$.)} # in case there is a column named `.`
  if(nrow(filtered) == 0){
    return(NULL)
  }
  filtered
}

filter_column <- function(x, type, pattern, min, max, ...){
  if(type == "string"){
    # The pattern must not be treated as a regular expression
    stringr::str_detect(x, paste0("\\Q", pattern, "\\E")) 
  } else{
    x >= min & x <= max
  }
}

