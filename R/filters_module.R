filters_ui <- function(id) {
  ns <- NS("id")
  div(id = "filter_container")
}

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
