input_append_attributes <- function(tag, ...) {
  tag$children[[2]] <- tagAppendAttributes(tag$children[[2]], ...)
  tag
}

label_append_attributes <- function(tag, ...) {
  tag$children[[1]] <- tagAppendAttributes(tag$children[[1]], ...)
  tag
}

v_numeric_input <- function(inputId, label, value, min = NA, max = NA, 
                            step = NA, width = NULL, ...) {
  numericInput(inputId, label = label, value = value, min = min, max = max,
               step = step, width = width) %>%
    input_append_attributes(class = "v_numeric_input", ...)
}

