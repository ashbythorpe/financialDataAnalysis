#' View a data frame.
#' 
#' A shiny module that allows the user to view some data, and download it in
#' various file formats. The user can choose which columns are displayed, 
#' although the number of columns are limited to 15.
#' 
#' @param id The namespace of the module.
#' @param data The data to be viewed, filtered and sorted.
#' @param filters A table of filters to apply to the data.
#' 
#' @details 
#' The data is displayed using [reactable::reactable()], and the columns are
#' sortable and filterable.
#' 
#' @name data_module
#' @export
data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "box_row view_data_row",
      selectizeInput(ns("columns"), "Columns", choices = c(""), multiple = TRUE,
                     width = "60%", options = list(maxItems = 15)),
      div(
        class = "box_row",
        downloadButton(ns("download_csv"), "Download CSV"),
        downloadButton(ns("download_tsv"), "Download TSV"),
        downloadButton(ns("download_excel"), "Download Excel")
      )
    ),
    reactable::reactableOutput(ns("data"))
  )
}

#' @name data_module
#' @export
data_server <- function(id, data, filters) {
  moduleServer(id, function(input, output, session) {
    observe({
      updateSelectInput(
        session, "columns", choices = colnames(data()),
        selected = colnames(data())[purrr::map_lgl(data(), is.numeric)]
      )
    })
    
    filtered_data <- reactive({
      apply_filters(data(), filters())
    })
    
    selected_data <- reactive({
      req(data())
      if(length(input$columns) < 1 || 
         !any(input$columns %in% colnames(filtered_data()))) {
        NULL
      } else {
        filtered_data()[,input$columns]
      }
    })
    
    output$data <- reactable::renderReactable({
      req(selected_data())
      reactable::reactable(selected_data(), filterable = TRUE, compact = TRUE)
    })
    
    output$download_csv <- downloadHandler(
      filename = "FDA_data.csv",
      content = function(file) {
        vroom::vroom_write(selected_data(), file, delim = ",")
      }
    )
    
    output$download_tsv <- downloadHandler(
      filename = "FDA_data.tsv",
      content = function(file) {
        vroom::vroom_write(selected_data(), file)
      }
    )
    
    output$download_excel <- downloadHandler(
      filename = "FDA_data.xlsx",
      content = function(file) {
        writexl::write_xlsx(selected_data(), file)
      }
    )
  })
}
