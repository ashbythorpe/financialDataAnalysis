#' Main server
#' 
#' The main server logic for the application.
#' 
#' @param input A [shiny::reactiveValues()] object containing all inputs sent
#'   from the client/UI to the server.
#' @param output A "shinyoutput" object containing all values sent from the
#'   server to the client/UI.
#' @param session A "ShinySession" object. This is an environment that contains
#'   information and functionality relating to the current session (see 
#'   [shiny::session]).
#' 
#' @export
server <- function(input, output, session) {
  settings <- settings_server("settings")
  
  data <- data_input_server("data_input")
  
  scores <- create_scores_server("create_scores", data = data, 
                                 interactive = settings$interactive)
  
  scored_data <- reactive({
    apply_scores(data(), scores()) %>%
      score_final(scores())
  })
  
  view_data_server("view_data", scored_data, scores)
  
  forecast_price_server("forecast_price", settings$interactive)
  
  plot_data_server("plot_data", scored_data, scores, settings$interactive)
}
