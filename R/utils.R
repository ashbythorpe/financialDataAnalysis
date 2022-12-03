fix_names <- function(df, scores) {
  new_names <- vctrs::vec_as_names(c(scores$score_name, colnames(df)), quiet = T)
}

parent_ns <- function(id){
  id %>%
    stringr::str_remove(paste0("\\Q", ns.sep, "\\E[:alpha:]+_[:digit:]+$")) %>%
    NS()
}

add_info <- function(tag, id, ...) {
  ns_id <- paste0("tooltip-", id)
  div(class = "box_row info_row", tag, ..., 
      icon("circle-info", id = ns_id, tabIndex = 0))
}

add_info_to_label <- function(tag, id, ...) {
  tag$children[[1]] <- add_info(tag$children[[1]], id, ...)
  tag
}
