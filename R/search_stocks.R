#' @export
search_stocks <- function(x, pattern){
  if(is.null(pattern) || pattern == ""){
    return(x)
  }
  final_data <-
    stringr::str_split(pattern, " ")[[1]] %>%
    purrr::map(search_pattern, x = x) %>%
    purrr::reduce(`&`) %>%
    {x[.,]}
  if(nrow(final_data) == 0){
    return(NULL)
  }
  final_data
}

search_pattern <- function(x, pattern){
  stringr::str_detect(paste0("\\Q", stringr::str_to_lower(x$symbol), "\\E"), stringr::str_to_lower(pattern)) |
    stringr::str_detect(paste0("\\Q", stringr::str_to_lower(x$companyName), "\\E"), stringr::str_to_lower(pattern))
}

search_results <- function(x){
  if(is.null(x)){
    return(x)
  }
  dplyr::arrange(x, dplyr::desc(favourite)) %>%
    dplyr::select(symbol, companyName)
}
