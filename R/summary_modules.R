col_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$label("Column summary", `for` = "summary"),
    textOutput(ns("summary")),
    br()
  )
}

col_summary_server <- function(id, column) {
  moduleServer(id, function(input, output, session) {
    # Summarise the given column
    output$summary <- reactive({
      if(!is.null(column())) {
        col_summary(column())
      }
    })
  })
}

score_summary_ui <- function(id) {
  ns <- NS(id)
  plotly::plotlyOutput(ns("plot"))
}

score_summary_server <- function(id, column, score_spec) {
  moduleServer(id, function(input, output, session) {
    # Make sure the plot doesn't generate unnecessarily
    spec <- debounce(score_spec, 1200)
    
    # Create the score
    score <- reactive({
      req(spec(), column())
      if(spec()$score_type == "Custom coordinates") {
        # Mimic behaviour of `purrr::pmap()` when dealing with list columns
        custom_args <- spec()$custom_args[[1]]
        # Supply the custom_args separately since it is a list column
        rlang::inject(score_column(
          column(), custom_args = custom_args,
          !!!spec()[,colnames(spec()) != "custom_args"]
        ))
      } else {
        rlang::inject(
          score_column(column(), !!!spec())
        )
      }
    })
    
    # Create the plot summarising the score
    output$plot <- plotly::renderPlotly({
      req(column(), score())
      
      data <- tibble::tibble(
        column = column(),
        score = score()
      )
      
      # Create the plot
      p <- ggplot2::ggplot(data, ggplot2::aes(x = column, y = score)) +
        ggplot2::geom_line() +
        ggplot2::ylim(0,1) + # Make sure the y axis always has the same scale
        ggplot2::labs(x = "Column value", y = "Score") +
        ggplot2::ggtitle("Score distribution")
      
      plotly::ggplotly(p) # Makes the plot interactive
    })
  })
}
