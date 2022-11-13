forecast_price_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      width = 12,
      search_bar_ui(ns("search_bar"))
    ),
    box(
      width = 12,
      stock_summary_ui(ns("stock_summary"))
    ),
    box(
      width = 12,
      predict_price_ui(ns("predict_price"))
    )
  )
}

forecast_price_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    selected_stock <- search_bar_server("search_bar")
    
    stock_summary_server("stock_summary", selected_stock)
    
    predict_price_server("predict_price", selected_stock)
  })
}
