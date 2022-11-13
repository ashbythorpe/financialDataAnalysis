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
