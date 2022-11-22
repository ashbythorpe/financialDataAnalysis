#' Create and display a plot
#' 
#' A shiny module which creates and shows the plot that the user has specified.
#' 
#' @param id The namespace of the module.
#' @param type The plot type.
#' @param data The data to plot.
#' @param scores The data frame of score specifications created by the user. See
#'   [scores_init].
#' @param performance_col If `type` is "Score performance", the column to plot
#'   against the scores.
#' @param custom_type If `type` is "Custom", the plotting method of the custom
#'   plot.
#' @param custom_args If `type` is "Custom", the arguments to create the custom
#'   plot.
#' @param create The button input that specifies that a plot should be 
#'   generated.
#'   
#' @name plot_module
#' @export
plot_ui <- function(id) {
  ns <- NS(id)
  plotly::plotlyOutput(ns("plot"))
}

#' @name plot_module
#' @export
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
