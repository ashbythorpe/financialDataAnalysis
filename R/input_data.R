#' @export
input_data <- function(files, default_data = default_stock_data, combine = FALSE){
  if(is.null(files) || is.null(combine)){
    return(default_data)
  }
  dfs <- read_files(files)
  transformed_df <- combine_if_multiple(dfs) %>%
    combine_if_specified(default_data, combine) %>%
    transform_df(default_data)
  error <- get_error(files, dfs, transformed_df)
  output_error(error)
  validate_df(transformed_df, default_data, error$fatal)
}

output_error <- function(error){
  if(error$fatal != ""){
    print(error$fatal)
  }
  if(error$nonfatal != ""){
    print(error$nonfatal)
  }
}

read_files <- function(files){
  dfs <- purrr::map(files, read_file)
  if(purrr::every(dfs, is.null)){
    return(NULL)
  }
  purrr::compact(dfs)
}

read_file <- function(file){
  if(is.na(file)){
    return(NULL)
  }
  format <- file_format(file)
  tryCatch(
    {
      if(format == "CSV"){
        suppressMessages(readr::read_csv(file, show_col_types = F, progress = F))
      } else if(format == "Excel"){
        suppressMessages(readxl::read_excel(file, progress = F))
      } else{
        suppressMessages(readr::read_delim(file, show_col_types = F, progress = F))
      }
    },
    error = function(cond){
      NULL
    }
  )
}

file_format <- function(file){
  excel_formats <- c("\\.xls$", "\\.xlsx$", "\\.xlsm$", "\\.xltx$", "\\.xltm$")
  if(stringr::str_detect(file, "\\.csv$")){
    "CSV"
  } else if(purrr::some(excel_formats, stringr::str_detect, string = file)){
    "Excel"
  } else{
    "not recognised"
  }
}

combine_if_multiple <- function(dfs){
  if(is.null(dfs)){
    return(dfs)
  }
  purrr::reduce(dfs, combine_two_dfs, .init = NULL)
}

combine_two_dfs <- function(df_1, df_2){
  if(is.null(df_1)){
    df_2
  } else if(is.null(df_2)){
    df_1
  } else if(any(colnames(df_1) %in% colnames(df_2))){
    suppressMessages(dplyr::full_join(df_1, df_2))
  } else if(nrow(df_1) == nrow(df_2)){
    dplyr::bind_cols(df_1, df_2)
  } else{
    dplyr::bind_rows(df_1, df_2)
  }
}

combine_if_specified <- function(df, default, combine){
  if(!combine){
    return(df)
  }
  combine_two_dfs(df, default)
}

transform_df <- function(df, default){
  if(is.null(df)){
    return(NULL)
  } else if(identical(df, default)){
    return(df)
  }
  res <- df %>%
    dplyr::distinct() %>%
    purrr::modify(transform_col)
  if(all(!purrr::map_lgl(res, is.numeric) | purrr::map_lgl(res, purrr::every, is.na))){
    return(NULL)
  }
  res
}

transform_col <- function(x){
  if(!is.numeric(x) && mean(!is.na(suppressWarnings(as.numeric(x)))) >= 0.5){
    suppressWarnings(as.numeric(x))
  } else if(is.numeric(x)) {
    x
  } else {
    as.character(x)
  }
}

get_error <- function(files, dfs, current_df){
  fatal <- ""
  nonfatal <- ""
  if(!is.null(dfs) && length(files) > length(dfs)){
    nonfatal = "Not all files were converted correctly."
  }
  if(is.null(dfs)){
    fatal <- "Files were not converted correctly."
  } else if(is.null(current_df)){
    fatal <- "The data does not contain any scorable columns."
  } else if(any(dim(current_df) < 2)){
    fatal <- "The inputted data frame is not valid."
  }
  list(fatal = fatal, nonfatal = nonfatal)
}

validate_df <- function(df, default, error){
  if(is.null(df) || error != ""){
    default
  } else{
    df
  }
}

