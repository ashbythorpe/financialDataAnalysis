plot_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      width = 6,
      selectInput(ns("plot_select"), "Select plot", choices = c(
        "Score distributions", "Score performance", "Custom"
      )),
      conditionalPanel(
        "input.plot_select == 'Custom'", ns = ns,
        custom_plot_ui(ns("custom_plot"))
      )
    ),
    box(
      width = 6,
      plot_ui(ns("plot"))
    )
  )
}

plot_data_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    custom_plot <- reactive(input$plot_select == "Custom")
    
    custom_args <- custom_plot_server("custom_plot", custom_plot, data)
    
    plot_server("plot", custom_plot, custom_args)
  })
}
