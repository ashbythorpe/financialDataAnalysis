download_df <- function(df, file_type, file){
  if(is.null(df) || is.null(file_type) || !file_type %in% c("CSV", "Excel") || is.null(file)){
    return(NULL)
  } else if(file_type == "CSV"){
    readr::write_csv(df, file)
  } else{
    writexl::write_xlsx(df, file)
  }
}

