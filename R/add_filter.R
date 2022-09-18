initial_filters <- function(){
  tibble::tibble(
    type = character(),
    colname = character(),
    pattern = character(),
    min = numeric(),
    max = numeric(),
    .rows = 0
  )
}

#' @export
add_filter <- function(filters, colname, data){
  if(is.null(colname) || !colname %in% colnames(data)){
    return(filters)
  }
  column <- data[[colname]]
  if(is.character(column)){
    filter <- tibble::tibble_row(type = "character", colname = colname, pattern = "")
  } else{
    filter <- tibble::tibble_row(type = "numeric", colname = colname,
                                 min = min(column, na.rm = T),
                                 max = max(column, na.rm = T))
  }
  tibble::add_row(filters, filter)
}

#' @export
edit_filter <- function(filters, x, pattern = NULL, min = NULL, max = NULL){
  if(x > nrow(filters)){
    return(filters)
  }
  type <- filters[x,]$type
  if(type == "character"){
    edit_character_filter(filters, x, pattern)
  } else{
    edit_numeric_filter(filters, x, min, max)
  }
}

edit_character_filter <- function(filters, x, pattern){
  if(is.null(pattern)){
    return(filters)
  }
  filters[x, "pattern"] <- pattern
  filters
}

edit_numeric_filter <- function(filters, x, min, max){
  if(is.null(min) || is.null(max)){
    return(filters)
  }
  filters[x, "min"] <- min
  filters[x, "max"] <- max
  filters
}

#' @export
remove_filter <- function(filters, x){
  if(x > nrow(filters)){
    return(filters)
  }
  dplyr::slice(filters, -x)
}

