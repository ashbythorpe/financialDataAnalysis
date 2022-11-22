#' Input custom coordinates to create a custom score
#' 
#' A shiny module that allows the user to specify the custom coordinates for a
#' custom score.
#' 
#' @inheritParams linear_score_module
#' 
#' @details 
#' The module's initial state uses two coordinates to create a linear score.
#' 
#' @returns 
#' The server returns a validated [tibble::tibble()] of coordinates.
#' 
#' @seealso [custom_row_module]
#' 
#' @name custom_score_module
#' @export
custom_score_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      id = "custom_row_container",
      # Start with two "default" rows
      custom_row_ui(ns("row_1"), 1),
      custom_row_ui(ns("row_2"), 2, y = 1)
    ),
    br()
  )
}

#' @name custom_score_module
#' @export
custom_score_server <- function(id, column, reset, editing_row){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    values <- reactiveValues()
    
    # Container for module outputs
    module_outputs <- reactiveValues()
    
    # A vector of row numbers
    # Each number corresponds to an `n` in the custom_row_ui() function
    # The order of this vector is the same as the order of the rows
    values$rows <- c(1,2)
    
    module_outputs[["1"]] <- custom_row_server("row_1")
    module_outputs[["2"]] <- custom_row_server("row_2")
    
    # Get the minimum and maximum of the column
    minc <- reactive(min(column(), na.rm = T))
    maxc <- reactive(max(column(), na.rm = T))
    
    # Initially, update the two rows to create a 'default' score.
    # This should be the same as the default linear score
    observe({
      updateNumericInput(session, paste0("row_1", ns.sep, "x"), value = minc())
      updateNumericInput(session, paste0("row_2", ns.sep, "x"), value = maxc())
    })
    
    # Create a new row
    # input$add is the location after which to put the new row
    observe({
      # Protect against the rare case of integer overflow
      if(max(values$rows) < .Machine$integer.max) {
        # Guarantee a new value of n
        n <- max(values$rows) + 1
        # Number seems to be converted to a string from JS to R, hence as.numeric()
        row <- which(values$rows == as.numeric(input$add))
        # The jQuery selector for the specified row.
        selector <- paste0("row_", input$add) %>%
          ns() %>%
          paste0("#", .)
        # Insert a new row after the specified row.
        insertUI(selector, where = "afterEnd",
                 custom_row_ui(ns(paste0("row_", n)), n, x = minc()))
        # Update values$scores to include this new row.
        values$rows <- append(values$rows, n, after = row)
        module_outputs[[as.character(n)]] <- custom_row_server(paste0("row_", n))
      }
    }) %>%
      bindEvent(input$add)
    
    # Delete a row
    # input$delete is the number of the row to delete
    observe({
      # Don't delete a row if there is only one left
      if(length(values$rows) > 1 && input$delete %in% values$rows) {
        selector <- paste0("row_", input$delete) %>%
          ns() %>%
          paste0("#", .)
        removeUI(selector)
        # Remove the row from values$rows
        values$rows <- values$rows[values$rows != as.numeric(input$delete)]
        module_outputs[[as.character(input$delete)]] <- NULL
      }
    }) %>%
      bindEvent(input$delete)
    
    # Reset coordinate rows to original state when reset() changes
    observe({
      # Remove all existing rows
      removeUI(".custom_row", multiple = TRUE)
      purrr::walk(values$rows, ~ {
        module_outputs[[as.character(.)]] <- NULL
      })
      
      # Add default row
      insertUI("#custom_row_container", where = "beforeEnd",
               tagList(
                 custom_row_ui(ns("row_1"), 1, x = minc()),
                 custom_row_ui(ns("row_2"), 2, x = maxc(), y = 1)
               ))
      values$rows <- c(1,2)
      module_outputs[["1"]] <- custom_row_server("row_1")
      module_outputs[["2"]] <- custom_row_server("row_2")
    }) %>%
      bindEvent(reset(), ignoreInit = TRUE)
    
    # Change state to score currently being edited.
    observe({
      if(identical(editing_row()$score_type, "Custom coordinates")) {
        # Extract the coordinates
        coords <- editing_row()$custom_args[[1]]
        
        # Remove all existing rows
        removeUI(".custom_row", multiple = TRUE)
        purrr::walk(as.character(values$rows), ~ {
          module_outputs[[.]] <- NULL
        })
        
        # Create a table of arguments to pass to custom_row_ui
        args <- tibble::tibble(
          n = seq_len(nrow(coords)),
          id = paste0("row_", n) %>%
            purrr::map_chr(ns),
          !!!coords
        )
        
        # Add new elements as specified
        ui_elements <- purrr::pmap(args, custom_row_ui) %>%
          tagList()
        insertUI("#custom_row_container", where = "beforeEnd", ui_elements)
        values$rows <- seq_len(nrow(coords))
        purrr::walk(as.character(values$rows), ~ {
          module_outputs[[.]] <- custom_row_server(paste0("row_", .))
        })
      }
    }) %>%
      bindEvent(editing_row(), ignoreInit = T)
    
    custom_args <- reactive({
      purrr::map(as.character(values$rows), ~ {module_outputs[[.]]}) %>%
        purrr::map(rlang::exec) %>% # Get the values from the expressions
        dplyr::bind_rows() # Combine them
    }) %>%
      bindEvent(values$rows, input$update)
    
    # Validate the custom coordinates
    custom_row <- reactive({
      validate_custom_score(custom_args())
    })
    
    custom_row
  })
}

#' Input a set of coordinates for a custom score
#' 
#' A shiny module that contains the UI and server logic for a single row (or a
#' single set of coordinates) of a custom score.
#' 
#' @param id The namespace of the module.
#' @param n The row number - unique for every row.
#' @param x The initial value of the x coordinate.
#' @param y The initial value of the y coordinate.
#' 
#' @details 
#' Note that the row number (`n`) may not reflect the actual position of the row
#' within the app. As an example, consider three rows (`n` is 1, 2 and 3 
#' respectively). If the second row is deleted, since the UI is not changed, the
#' row which will now be second will have a row number of 3.
#' 
#' @returns 
#' The server returns a [tibble::tibble_row()] containing the x and y 
#' coordinates.
#' 
#' @seealso [custom_score_module]
#' 
#' @name custom_row_module
#' @export
custom_row_ui <- function(id, n, x = 0, y = 0) {
  ns <- NS(id)
  # Get the namespace of the 'custom_score' module.
  # This is so we can send messages to it using JavaScript.
  p_ns <- parent_ns(id)
  
  div(
    # the 'custom_row' class allows all these rows to be deleted using jQuery
    # selectors
    id = id, class = "custom_row box_row",
    
    # Remove the label
    numericInput(ns("x"), label = "", value = x)$children[[2]] %>%
      # See 'inst/assets/script.js'
      # The JavaScript will alert the custom_score_server when the inputs of 
      # any row are updated
      tagAppendAttributes(class = "custom_update",
                          `data-id` = p_ns("update")),
    
    # Better enforcement of min and max
    v_numeric_input(ns("y"), "", min = 0, max = 1, value = y)$children[[2]] %>%
      tagAppendAttributes(class = "custom_update",
                          `data-id` = p_ns("update")),
    # p_ns('delete') corresponds to input$delete in custom_score_server
    actionButton(
      ns("unused2"), "", icon = icon("trash"), class = "delete_row", 
      `data-n` = n, `data-id` = p_ns("delete")
    ),
    # The corresponding JavaScript will alert the custom_score server when
    # a button is pressed, and tell the server which row this happened in 
    # (data-n)
    actionButton(
      ns("unused"), "", icon = icon("plus"), class = "add_row", 
      `data-n` = n, `data-id` = p_ns("add")
    )
  )
}

#' @name custom_row_module
#' @export
custom_row_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Create a row with the specified x and y coordinates
    row <- reactive({
      req(input$x, input$y)
      if(input$y > 1) {
        y <- 1
      } else if(input$y < 0) {
        y <- 0
      } else {
        y <- input$y
      }
      tibble::tibble_row(x = input$x, y = y)
    })
    
    # Return the row
    row
  })
}
