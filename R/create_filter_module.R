create_filter_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("colname"), label = "Column to filter:", choices = c("")),
    actionButton(ns("create"), "Create filter", icon = icon("square-plus"))
  )
}

create_filter_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    valid_colnames <- reactive({
      colnames(data())[purrr::map_lgl(data(), is.numeric)]
    })
    
    observe({
      updateSelectInput(session, "colname", choices = valid_colnames(),
                        selected = valid_colnames()[1])
    }) %>%
      bindEvent(data())
    
    add <- reactive({
      if(isTRUE(input$colname %in% valid_colnames())) {
        input$colname
      } else {
        NULL
      }
    }) %>%
      bindEvent(input$create)
    
    add
  })
}
