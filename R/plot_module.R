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
#' @param show_text Passed into [correlation_plot()].
#' @param custom_type If `type` is "Custom", the plotting method of the custom
#'   plot.
#' @param custom_args If `type` is "Custom", the arguments to create the custom
#'   plot.
#' @param create The button input that specifies that a plot should be 
#'   generated.
#' @param interactive Whether the created plot should be interactive.
#'   
#' @name plot_module
#' @export
plot_ui <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    type = "hidden", id = ns("wizard"),
    tabPanelBody(
      "interactive",
      div(
        id = "plot_waiter", 
        plotly::plotlyOutput(ns("plot"))
      )
    ),
    tabPanelBody(
      "normal",
      waiter::withWaiter(
        plotOutput(ns("normal_plot")),
        waiter::spin_three_bounce(), color = "white"
      )
    )
  )
}

#' @name plot_module
#' @export
plot_server <- function(id, type, data, scores, performance_col, show_text, 
                        custom_type, custom_args, create, interactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    waiter <- waiter::Waiter$new(
      id = "plot_waiter", color = "white",
      waiter::spin_three_bounce()
    )
    
    interactive_value <- reactive({
      x <- tryCatch(interactive(), error = function(c) TRUE)
      if(!is.logical(x)) {
        x <- TRUE
      }
      x
    })
    
    observe({
      if(!interactive_value()) {
        updateTabsetPanel(session, "wizard", selected = "normal")
      } else {
        updateTabsetPanel(session, "wizard", selected = "interactive")
      }
    })
    
    plot <- reactive({
      req(type())
      if(isTruthy(type())) {
        waiter$show()
      }
      if(type() == "Score distributions") {
        score_distributions(get_scores(data(), scores()))
      } else if(type() == "Score performance") {
        req(performance_col())
        score_performance(data(), performance_col(),
                          get_scores(data(), scores()))
      } else if(type() == "Correlation heatmap") {
        correlation_plot(data(), show_text())
      } else {
        custom_plot(data(), custom_type(), !!!custom_args(),
                    .interactive = interactive_value())
      }
    }) %>%
      bindEvent(create())
    
    output$plot <- plotly::renderPlotly({
      req(plot(), !identical(interactive_value(), FALSE))
      p <- plotly::ggplotly(plot(), source = "plot")
      plotly::event_register(p, "plotly_afterplot")
      if(type() == "Custom") {
        build_plot(p)
      } else {
        p
      }
    }) %>%
      bindEvent(plot())
    
    observe({
      if(!isTruthy(tryCatch(plot(), error = function(c) FALSE))) {
        waiter$hide()
      }
    })
    
    observe({
      waiter$hide()
    }) %>%
      bindEvent({
        req(plot(), !identical(interactive_value(), FALSE))
        plotly::event_data("plotly_afterplot", source = "plot", 
                           priority = "event")
      })
    
    output$normal_plot <- renderPlot({
      if(type() == "Custom") {
        print_plot(plot())
      } else {
        plot()
      }
    })
  })
}
