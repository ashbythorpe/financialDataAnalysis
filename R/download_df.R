download_df <- function(df, file_type, file){
  if(is.null(df) || is.null(file_type) || !file_type %in% c("CSV", "Excel") || is.null(file)){
    return(NULL)
  } else if(file_type == "CSV"){
    suppressMessages(readr::write_csv(df, file, progress = FALSE))
  } else{
    suppressMessages(writexl::write_xlsx(df, file))
  }
}
