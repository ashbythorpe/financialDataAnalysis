#' Convert a set of files into a data frame
#'
#' Read one or more files and combine them into a single data frame. If the
#' result is invalid, the default stock data is returned
#'
#' @param files A vector of file paths. The function only accepts CSV and Excel
#'   files.
#' @param default_data The default data, included for testing purposes. It is
#'   not recommended to change this from its default.
#' @param combine If `TRUE`, the resulting data frame will be combined with the
#'   default data frame.
#'
#' @returns A [tibble::tibble()].
#'
#' @seealso [default_stock_data]
#'
#' @examples
#' file <- tempfile(fileext = ".csv")
#' data <- tibble::tibble(
#'   x = 1:100,
#'   y = 100:1
#' )
#' vroom::vroom_write(data, file, ",")
#'
#' input_data(file)
#'
#' @export
input_data <- function(files, default_data = default_stock_data, combine = FALSE) {
  if (is.null(files) || is.null(combine)) {
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

output_error <- function(error) {
  if (error$fatal != "") {
    print(error$fatal)
  }
  if (error$nonfatal != "") {
    print(error$nonfatal)
  }
}

read_files <- function(files) {
  dfs <- purrr::map(files, read_file)
  if (purrr::every(dfs, is.null)) {
    return(NULL)
  }
  purrr::compact(dfs)
}

read_file <- function(file) {
  if (is.na(file)) {
    return(NULL)
  }
  format <- file_format(file)
  tryCatch(
    {
      if (format == "Excel") {
        readxl::read_excel(file, progress = F)
      } else if (format == "DTA") {
        haven::read_dta(file)
      } else if (format == "SPSS") {
        haven::read_spss(file)
      } else if (format == "SAS") {
        haven::read_sas(file)
      } else if (format == "XPT") {
        haven::read_xpt(file)
      } else {
        vroom::vroom(file, progress = FALSE, show_col_types = FALSE)
      }
    },
    error = function(cond) {
      NULL
    }
  )
}

file_format <- function(file) {
  ext <- stringr::str_to_lower(tools::file_ext(file))
  if (ext %in% c("gz", "bz2", "xz", "zip")) {
    new_file <- stringr::str_remove(file, paste0(".", ext, "$"))
    return(file_format(new_file))
  }
  excel_formats <- c("xls", "xlsx", "xlsm", "xltx", "xltm")
  if (ext %in% c("csv", "tsv", "fwf")) {
    "Delimited"
  } else if (ext %in% excel_formats) {
    "Excel"
  } else if (ext == "dta") {
    "DTA"
  } else if (ext %in% c("sav", "zsav", "por")) {
    "SPSS"
  } else if (ext %in% c("sas7bdat", "sas7bcat")) {
    "SAS"
  } else if (ext == "xpt") {
    "XPT"
  } else {
    "not recognised"
  }
}

combine_if_multiple <- function(dfs) {
  if (is.null(dfs)) {
    return(dfs)
  }
  purrr::reduce(dfs, combine_two_dfs, .init = NULL)
}

combine_two_dfs <- function(df_1, df_2) {
  if (is.null(df_1) || 0 %in% dim(df_1)) {
    return(df_2)
  } else if (is.null(df_2) || 0 %in% dim(df_2)) {
    return(df_1)
  }

  # Names that are in both columns
  equal_names <- generics::intersect(colnames(df_1), colnames(df_2))

  if (length(equal_names) > 0) {
    # Get names of compatible columns (where a common type exists)
    compatible_names <- purrr::keep(equal_names, ~ {
      tryCatch(
        {
          # Check that the columns can be combined
          vctrs::vec_ptype_common(df_1[[.]], df_2[[.]])
          TRUE
        },
        error = function(c) {
          FALSE
        }
      )
    })
  }

  if (length(equal_names) > 0 && length(compatible_names) > 0) {
    suppressMessages(dplyr::full_join(df_1, df_2, by = compatible_names))
  } else if (nrow(df_1) == nrow(df_2)) {
    dplyr::bind_cols(df_1, df_2)
  } else if (length(equal_names) > 0) {
    # Prevent bind_rows() from trying to combine incompatible columns.
    colnames(df_1)[colnames(df_1) %in% equal_names] <-
      paste0(colnames(df_1)[colnames(df_1) %in% equal_names], ".x")
    colnames(df_2)[colnames(df_2) %in% equal_names] <-
      paste0(colnames(df_2)[colnames(df_2) %in% equal_names], ".y")
    dplyr::bind_rows(df_1, df_2)
  } else {
    dplyr::bind_rows(df_1, df_2)
  }
}

combine_if_specified <- function(df, default, combine) {
  if (!combine) {
    return(df)
  }
  combine_two_dfs(df, default)
}

transform_df <- function(df, default) {
  if (is.null(df)) {
    return(NULL)
  } else if (identical(df, default)) {
    return(df)
  }
  res <- df %>%
    dplyr::distinct() %>%
    purrr::modify(transform_col)
  if (all(!purrr::map_lgl(res, is.numeric) | purrr::map_lgl(res, purrr::every, is.na))) {
    return(NULL)
  }
  res
}

transform_col <- function(x) {
  if (!is.numeric(x) && mean(!is.na(suppressWarnings(as.numeric(x)))) >= 0.5) {
    suppressWarnings(as.numeric(x))
  } else if (is.numeric(x)) {
    x
  } else {
    as.character(x)
  }
}

get_error <- function(files, dfs, current_df) {
  fatal <- ""
  nonfatal <- ""
  if (!is.null(dfs) && length(files) > length(dfs)) {
    nonfatal <- "Not all files were converted correctly."
  }
  if (is.null(dfs)) {
    fatal <- "Files were not converted correctly."
  } else if (is.null(current_df)) {
    fatal <- "The data does not contain any scorable columns."
  } else if (any(dim(current_df) < 2)) {
    fatal <- "The inputted data frame is not valid."
  }
  list(fatal = fatal, nonfatal = nonfatal)
}

validate_df <- function(df, default, error) {
  if (is.null(df) || error != "") {
    default
  } else {
    df
  }
}
