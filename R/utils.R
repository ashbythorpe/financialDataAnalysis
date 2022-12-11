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

# Combine two shiny events
combine_events <- function(..., .reject_invalid = c("all", "any")) {
  .reject_invalid <- rlang::arg_match(.reject_invalid)
  qs <- rlang::enquos0(...)
  vals <- lapply(qs, exec_event_safely)
  if(.reject_invalid == "all" && all(sapply(vals, event_is_invalid))) {
    # Return NULL if all other events are NULL
    NULL
  } else if (.reject_invalid == "any" && any(sapply(vals, event_is_invalid))) {
    # Return NULL if any events are invalid
    NULL
  } else {
    vals
  }
}

exec_event_safely <- function(x) {
  try(rlang::eval_tidy(x), silent = TRUE)
}

event_is_invalid <- function(x) {
  inherits(x, "try-error") || is.null(x)
}
