#' Write a data frame to a file
#' 
#' Take a data frame and write it to a CSV (.csv) or Excel (.xlsx) file.
#' 
#' @param df The data frame to write.
#' @param file_type The type of the file to create, either "CSV" or "Excel".
#' @param file A string. The file path to write to.
#' 
#' @returns The file path that was written to.
#' 
#' @examples 
#' file <- tempfile(fileext = "csv")
#' data <- tibble::tibble(
#'   x = 1:10
#' )
#' 
#' download_df(data, "CSV", file)
#' 
#' @export
download_df <- function(df, file_type, file){
  if(is.null(df) || is.null(file_type) || !file_type %in% c("CSV", "Excel") || is.null(file)){
    return(NULL)
  } else if(file_type == "CSV"){
    suppressMessages(readr::write_csv(df, file, progress = FALSE))
  } else{
    suppressMessages(writexl::write_xlsx(df, file))
  }
}
