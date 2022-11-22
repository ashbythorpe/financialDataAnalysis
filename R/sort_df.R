#' Sort a data frame by one of its columns
#' 
#' Arrange a daat frame by one of its columns, either in ascending or descending
#' order.
#' 
#' @param df The data frame to sort.
#' @param colname The column to sort with.
#' @param desc Whether to sort in descending order.
#' 
#' @returns `df`, sorted.
#' 
#' @examples 
#' data <- tibble::tibble(
#'   x = 10:1,
#'   y = c(letters[6:10], letters[5:1])
#' )
#' 
#' sort_df(data, "x")
#' sort_df(data, "y", desc = TRUE)
#'
#' @export
sort_df <- function(df, colname, desc = FALSE){
  if(is.null(df)){
    return(NULL)
  } else if(is.null(colname) || !colname %in% colnames(df) || is.null(desc)){
    df
  } else if(!desc){
    dplyr::arrange(df, .data[[colname]])
  } else{
    dplyr::arrange(df, dplyr::desc(.data[[colname]]))
  }
}
