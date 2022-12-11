#' Create a filter
#'
#' A shiny module containing a simple select input/action button combination.
#' The column to filter is selected, and the button is pressed to actually
#' create the filter.
#'
#' @param id The namespace of the module.
#' @param data The data that is being filtered.
#'
#' @returns
#' The server alerts its parent of the value of the select input whenever the
#' button is pressed. Note that the logic for adding filters is not contained
#' within this module.
#'
#' @seealso [add_filter()]
#'
#' @name create_filter_module
#' @export
create_filter_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("colname"), label = "Column to filter:", choices = c("")),
    actionButton(ns("create"), "Create filter", icon = icon("square-plus"))
  )
}

#' @name create_filter_module
#' @export
create_filter_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    valid_colnames <- reactive({
      colnames(data())[purrr::map_lgl(data(), is.numeric)]
    })

    observe({
      updateSelectInput(session, "colname",
        choices = valid_colnames(),
        selected = valid_colnames()[1]
      )
    }) %>%
      bindEvent(data())

    add <- reactive({
      if (isTRUE(input$colname %in% valid_colnames())) {
        input$colname
      } else {
        NULL
      }
    }) %>%
      bindEvent(input$create)

    add
  })
}
