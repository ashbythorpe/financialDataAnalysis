#' Search and select a stock.
#' 
#' A shiny module that allows the user to search through stocks using the ticker
#' and name of the stock.
#' 
#' @param id The namespace of the module
#' 
#' @returns 
#' The server returns the selected stock.
#' 
#' @name search_bar_module
#' @export
search_bar_ui <- function(id) {
  ns <- NS(id)
  shinyWidgets::pickerInput(
    ns("stock"), choices = default_stock_data$symbol,
    options = shinyWidgets::pickerOptions(
      liveSearch = TRUE, liveSearchPlaceholder = "Select a stock"
    ),
    choicesOpt = list(
      tokens = paste(default_stock_data$symbol, default_stock_data$company_name)
    ),
  )
}

#' @name search_bar_module
#' @export
search_bar_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    stock <- reactive({
      if(isTruthy(input$stock) && input$stock %in% default_stock_data$symbol) {
        input$stock
      } else {
        NULL
      }
    })
    
    stock
  })
}
