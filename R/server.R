server <- function(input, output, session) {
  data <- data_input_server("data_input")
  
  scores <- create_scores_server("create_scores", data = data)
  
  scored_data <- reactive({
    apply_scores(data(), scores()) %>%
      score_final(scores())
  })
  
  view_data_server("view_data", scored_data, scores)
  
  forecast_price_server("forecast_price")
  
  plot_data_server("plot_data", scored_data, scores)
}
