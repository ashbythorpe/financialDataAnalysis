data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "box_row",
      selectizeInput(ns("columns"), "Columns", choices = c(""), multiple = TRUE,
                     width = "80%", options = list(maxItems = 15)),
      downloadButton(ns("download_csv"), "Download CSV"),
      downloadButton(ns("download_excel"), "Download Excel")
    ),
    reactable::reactableOutput(ns("data"))
  )
}

data_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observe({
      updateSelectInput(
        session, "columns", choices = colnames(data()),
        selected = colnames(data())[purrr::map_lgl(data(), is.numeric)]
      )
    })
    
    selected_data <- reactive({
      req(data())
      if(length(input$columns) < 1 || !any(input$columns %in% colnames(data()))) {
        NULL
      } else {
        data()[,input$columns]
      }
    })
    
    output$data <- reactable::renderReactable({
      req(selected_data())
      reactable::reactable(selected_data(), filterable = TRUE, compact = TRUE)
    })
    
    output$download_csv <- downloadHandler(
      filename = "stock_data.csv",
      content = function(file) {
        readr::write_csv(selected_data(), file)
      }
    )
    
    output$download_excel <- downloadHandler(
      filename = "stock_data.xlsx",
      content = function(file) {
        writexl::write_xlsx(selected_data(), file)
      }
    )
  })
}