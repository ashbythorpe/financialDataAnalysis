#' @export
favourite_stock <- function(df, x){
  if(is.null(df)){
    return(NULL)
  } else if(is.null(x) || x > nrow(df)){
    return(df)
  }
  df[x, "favourite"] <- !df[x,]$favourite
  df
}
