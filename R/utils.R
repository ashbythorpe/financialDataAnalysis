fix_names <- function(df, scores) {
  new_names <- vctrs::vec_as_names(c(scores$score_name, colnames(df)), quiet = T)
}

parent_ns <- function(id){
  id %>%
    stringr::str_remove(paste0("\\Q", ns.sep, "\\E[:alpha:]+_[:digit:]+$")) %>%
    NS()
}

# https://github.com/rstudio/shiny/issues/1484#issuecomment-262812760
dedupe <- function(r) {
  makeReactiveBinding("val")
  observe(val <<- r(), priority = 10)
  reactive(val)
}

makeReactiveTrigger <- function() {
  rv <- reactiveValues(a = 0)
  list(
    depend = function() {
      rv$a
      invisible()
    },
    trigger = function() {
      rv$a <- isolate(rv$a + 1)
    }
  )
}

add_info <- function(tag, id, ...) {
  ns_id <- paste0("tooltip-", id)
  div(class = "box_row info_row", tag, icon("circle-info", id = ns_id), ...)
}

add_info_to_label <- function(tag, id, ...) {
  tag$children[[1]] <- add_info(tag$children[[1]], id, ...)
  tag
}
