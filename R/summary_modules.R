score_summary_ui <- function(id) {
  ns <- NS(id)
  plotly::plotlyOutput(ns("plot"))
}

score_summary_server <- function(id, data, score_spec) {
  moduleServer(id, function(input, output, session) {
    column <- reactive({
      req(score_spec())
      score_spec()[[score_spec()$colname]]
    })
    
    score <- reactive({
      req(score_spec())
      rlang::inject(
        score_column(column(), !!!score_spec)
      )
    })
    
    output$plot <- plotly::renderPlotly({
      req(column(), score())
      
      data <- tibble::tibble(
        `Column value` = column(),
        Score = score()
      )
      p <- ggplot2::ggplot(data, ggplot2::aes(x = column, y = score)) +
        ggplot2::geom_line() +
        ggthemes::theme_clean()
      
      plotly::ggplotly(p)
    })
  })
}
