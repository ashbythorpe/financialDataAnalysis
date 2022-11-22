#' Filter, sort and view scored data
#' 
#' A shiny module that contains the View Data page. Allows the user to create
#' filters (which are then applied to the data), sort their data and download
#' it in a variety of formats.
#' 
#' @param id The namespace of the module.
#' @param data The data frame, after the scoring process.
#' @param scores The data frame of score specifications created by the user. See
#'   [scores_init].
#' 
#' @name view_data_module
#' @export
view_data_ui <- function(id) {
  ns <- NS(id)
  div(
    style = "overflow-y: scroll", # Make this page scrollable
    fluidRow(
      box(
        width = 6,
        title = "Create filter",
        create_filter_ui(ns("create_filters"))
      ),
      box(
        width = 6,
        title = "Filters",
        filters_ui(ns("filters"))
      )
    ),
    box(
      width = 12,
      title = "Data",
      data_ui(ns("data"))
    )
  )
}

#' @name view_data_module
#' @export
view_data_server <- function(id, data, scores) {
  moduleServer(id, function(input, output, session) {
    add <- create_filter_server("create_filters", data)
    
    filters <- filters_server("filters", data, add)

    filtered_data <- reactive({
      apply_filters(data(), filters())
    })

    data_server("data", filtered_data)
  })
}
