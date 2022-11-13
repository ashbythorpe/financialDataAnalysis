search_bar_ui <- function(id) {
  ns <- NS(id)
  shinyWidgets::pickerInput(
    ns("stock"), choices = default_stock_data$symbol,
    options = shinyWidgets::pickerOptions(
      liveSearch = TRUE, liveSearchPlaceholder = "Select a stock"
    ),
    choicesOpt = list(
      tokens = paste(default_stock_data$symbol, default_stock_data$companyName)
    ),
  )
}

search_bar_server <- function(id) {
  stock <- reactive({
    if(isTruthy(input$stock) && input$stock %in% default_stock_data$symbol) {
      input$stock
    } else {
      NULL
    }
  })
  
  stock
}
