format_quietly_output <- function(x){
  glue::glue("[1] \"{x}\"")
}

parent_ns <- function(id){
  id %>%
    stringr::str_remove(paste0("\\Q", ns.sep, "\\Erow_[:digit:]+$")) %>%
    NS()
}

# https://github.com/rstudio/shiny/issues/1484#issuecomment-262812760
dedupe <- function(r) {
  makeReactiveBinding("val")
  observe(val <<- r(), priority = 10)
  reactive(val)
}
