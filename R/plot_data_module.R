plot_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      width = 6,
      selectInput(ns("plot_select"), "Select plot", choices = c(
        "Score distributions", "Score performance", "Custom"
      )),
      conditionalPanel(
        "input.plot_select == 'Score performance'", ns = ns,
        score_performance_ui(ns("score_performance"))
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

plot_data_server <- function(id, data, scores) {
  moduleServer(id, function(input, output, session) {
    custom_plot <- reactive(input$plot_select == "Custom")
    
    performance_col <- score_performance_server(
      "score_performance", data, scores
    )
    
    custom_module_output <- custom_plot_server("custom_plot", custom_plot, data)
    custom_type <- custom_module_output$type
    custom_args <- custom_module_output$args
    
    plot_server(
      "plot", type = reactive(input$plot_select), data = data, scores = scores, 
      performance_col = performance_col, custom_type = custom_type, 
      custom_args = custom_args, create = reactive(input$create)
    )
  })
}
