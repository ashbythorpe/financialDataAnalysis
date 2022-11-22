#' Edit a set of numeric filters
#' 
#' A shiny module that allows the user to edit a set of numeric filters for a 
#' data frame. 
#' 
#' @param id The namespace of the module.
#' @param data The data to filter.
#' @param add The input (from another module) that signifies the creation of a
#'   new filter.
#'   
#' @returns
#' The server returns a [tibble::tibble()] of filters.
#' 
#' @seealso [apply_filters()] [filter_module]
#' 
#' @name filters_module
#' @export
filters_ui <- function(id) {
  ns <- NS("id")
  div(id = "filter_container")
}

#' @name filters_module
#' @export
filters_server <- function(id, data, add) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    values <- reactiveValues()
    values$rows <- numeric()
    filter_outputs <- reactiveValues()
    
    observe({
      values$filters <- filters_init
    }) %>%
      bindEvent(data())
    
    observe({
      if(isTruthy(add())) {
        values$filters <- add_filter(values$filters, add(), data())
        column <- data()[[add()]]
        filter <- dplyr::slice_tail(values$filters)
        n <- max(c(0, values$rows)) + 1
        
        element <- filter_ui(ns(paste0("filter_", n)), n,
                             name = add(), column = column)
        
        insertUI("#filter_container", where = "beforeEnd",
                 element)
        values$rows <- c(values$rows, n)
        filter_outputs[[as.character(n)]] <- filter_server(paste0("filter_", n))
      }
    }) %>%
      bindEvent(add())
    
    observe({
      if(length(values$rows) >= 1 && input$delete %in% values$rows) {
        values$filters <- remove_filter(values$filters, as.numeric(input$delete))
        selector <- paste0("filter_", input$delete) %>%
          ns() %>%
          paste0("#", .)
        removeUI(selector)
        # Remove the row from values$rows
        values$rows <- values$rows[values$rows != as.numeric(input$delete)]
        filter_outputs[[as.character(input$delete)]] <- NULL
      }
    }) %>%
      bindEvent(input$delete)
    
    observe({
      removeUI(".filter", multiple = TRUE)
      
      purrr::walk(as.character(values$rows), ~ {
        module_outputs[[.]] <- NULL
      })
      values$rows <- numeric()
    }) %>%
      bindEvent(data(), ignoreInit = TRUE)
    
    filter_vals <- reactive({
      purrr::map(as.character(values$rows), ~ {filter_outputs[[.]]}) %>%
        purrr::map(rlang::exec) %>%
        dplyr::bind_rows()
    }) %>%
      bindEvent(input$update, values$rows) %>%
      throttle(1000)
    
    observe({
      if(nrow(values$filters) > 0 && nrow(filter_vals()) > 0) {
        min <- dplyr::coalesce(filter_vals()$min, values$filters$min)
        max <- dplyr::coalesce(filter_vals()$max, values$filters$max)
        if(!identical(values$filters$min, min)) {
          values$filters$min <- min
        }
        if(!identical(values$filters$max, max)) {
          values$filters$max <- max
        }
      }
    }) %>%
      bindEvent(filter_vals())
    
    reactive(values$filters)
  })
}

#' Edit a single numeric filter
#' 
#' A shiny module that allows the user to edit a single numeric filter. The
#' filter consists of a slider range input and two numeric inputs, where the
#' numeric inputs are linked to the values of the slider input.
#' 
#' @param id The namespace of the module.
#' @param n The filter number - unique for each filter.
#' @param name The column name that is being filtered.
#' @param column The column that is being filtered.
#' 
#' @details 
#' The link between the numeric and slider inputs means that when the numeric
#' inputs are updated, the slider inputs will update, and vice versa.
#' 
#' Note that the filter number (`n`) may not reflect the actual position of the 
#' filter within the app. As an example, consider three filters (`n` is 1, 2 and
#' 3 respectively). If the second filter is deleted, since the UI is not 
#' changed, the filter which will now be second will have a filter number of 3.
#' 
#' @returns 
#' The server returns a [tibble::tibble_row()] containing the min and max of the
#' filter.
#' 
#' @seealso [filters_module]
#' 
#' @name filter_module
#' @export
filter_ui <- function(id, n, name, column) {
  ns <- NS(id)
  p_ns <- parent_ns(id)
  minc <- min(column, na.rm = TRUE)
  maxc <- max(column, na.rm = TRUE)
  
  div(
    class = "filter", id = id,
    tags$label(paste0("Filtering \"", name, "\""), `for` = ns("filter_row")),
    div(
      # Use the framework already created in the custom_score_module()
      id = ns("filter_row"), class = "box_row",
      v_numeric_input(ns("min"), label = "", min = minc, max = maxc,
                      value = minc)$children[[2]] %>%
        tagAppendAttributes(class = "filter_update filter_min", 
                            `data-id` = p_ns("update"), `data-n` = n),
      
      sliderInput(ns("range"), label = "", min = minc, max = maxc,
                  value = c(minc, maxc), ticks = FALSE, width = "80%") %>%
        input_append_attributes(class = "filter_range", `data-n` = n,
                                onFinish = "slider_on_finish") %>%
        label_append_attributes(class = "unused_label"),
      
      v_numeric_input(ns("max"), label = "", min = minc, max = maxc,
                      value = maxc)$children[[2]] %>%
        tagAppendAttributes(class = "filter_update filter_max", 
                            `data-id` = p_ns("update"), `data-n` = n),
      actionButton(
        ns("unused2"), "", icon = icon("xmark"), class = "delete_filter", 
        `data-n` = n, `data-id` = p_ns("delete")
      ),
    )
  )
}

#' @name filter_module
#' @export
filter_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    p_ns <- session$ns("") %>%
      stringr::str_remove(paste0("\\Q", ns.sep, "\\E$")) %>%
      parent_ns()
    
    row <- reactive({
      if(isTruthy(input$min) && isTruthy(input$max) && input$min <= input$max) {
        tibble::tibble_row(
          min = input$min,
          max = input$max
        )
      } else {
        tibble::tibble_row(
          min = NA_real_,
          max = NA_real_
        )
      }
    })
    
    observe({
      if(isTruthy(input$min) && isTruthy(input$max) && input$min <= input$max) {
        session$sendCustomMessage("filters_update", p_ns("update"))
      }
    }) %>%
      bindEvent(input$min, input$max)
    
    row
  })
}
