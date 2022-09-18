server <- function(input, output, session) {
  data <- data_input_server("data_input")
  scores <- create_scores_server("create_scores", data = data())
}
