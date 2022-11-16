plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("plot"))
  )
}

plot_server <- function(id, type, data, scores, performance_col, custom_type, 
                        custom_args, create) {
  moduleServer(id, function(input, output, session) {
    output$plot <- plotly::renderPlotly({
      print("updated")
      req(type())
      if(type() == "Score distributions") {
        p <- score_distributions(get_scores(data(), scores()))
      } else if(type() == "Score performance") {
        req(performance_col())
        p <- score_performance(data(), performance_col(),
                               get_scores(data(), scores()))
      } else {
        p <- custom_plot(data(), custom_type(), !!!custom_args())
      }
      req(p)
      plotly::ggplotly(p) %>%
        print_plot()
    }) %>%
      bindEvent(create())
  })
}
