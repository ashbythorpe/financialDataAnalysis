#' @export
sort_df <- function(df, colname, desc){
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
