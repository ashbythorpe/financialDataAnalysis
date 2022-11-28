#' Create a variety of plots
#' 
#' A shiny module that contains the Plot Data page, which allows the user to 
#' create a variety of default or custom plots.
#' 
#' @param id The namespace of the module.
#' @param data The data to plot.
#' @param scores The data frame of score specifications created by the user. See
#'   [scores_init].
#' @param interactive Whether any plots created should be interactive.
#'   
#' @name plot_data_module
#' @export
plot_data_ui <- function(id) {
  ns <- NS(id)
  div(
    style = "overflow-y: scroll", # Make this page scrollable
    box(
      width = 6,
      selectInput(ns("plot_select"), "Select plot", choices = c(
        "Score distributions", "Score performance", "Correlation heatmap", 
        "Custom"
      )),
      conditionalPanel(
        "input.plot_select == 'Score performance'", ns = ns,
        score_performance_ui(ns("score_performance"))
      ),
      conditionalPanel(
        "input.plot_select == 'Correlation heatmap'", ns = ns,
        correlation_plot_ui(ns("correlation_plot"))
      ),
      conditionalPanel(
        "input.plot_select == 'Custom'", ns = ns,
        custom_plot_ui(ns("custom_plot"))
      ),
      actionButton(ns("create"), "Create plot")
    ),
    box(
      width = 6,
      plot_ui(ns("plot"))
    )
  )
}

#' @name plot_data_module
#' @export
plot_data_server <- function(id, data, scores, interactive) {
  moduleServer(id, function(input, output, session) {
    custom_plot <- reactive(input$plot_select == "Custom")
    
    performance_col <- score_performance_server(
      "score_performance", data, scores
    )
    
    show_text <- correlation_plot_server("correlation_plot")
    
    custom_module_output <- custom_plot_server("custom_plot", custom_plot, data)
    custom_type <- custom_module_output$type
    custom_args <- custom_module_output$args
    
    plot_server(
      "plot", type = reactive(input$plot_select), data = data, scores = scores, 
      performance_col = performance_col, show_text = show_text, 
      custom_type = custom_type, custom_args = custom_args, 
      create = reactive(input$create), interactive = interactive
    )
  })
}
