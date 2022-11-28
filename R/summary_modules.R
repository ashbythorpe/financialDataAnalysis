#' View a text summary of a column
#' 
#' A shiny module that displays a text summary of a selected numeric column.
#' 
#' @param id The namespace of the module.
#' @param column The column to summarise, in vector form.
#' 
#' @seealso [col_summary()]
#' 
#' @name col_summary_module
#' @export
col_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$label("Column summary", `for` = "summary"),
    textOutput(ns("summary")),
    br()
  )
}

#' @name col_summary_module
#' @export
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

#' View a graphical summary of a score
#' 
#' A shiny module that allows the user to view a plot of how the score they are
#' currently creating will look.
#' 
#' @param id The namespace of the module.
#' @param column The column being scored.
#' @param score_spec The score currently being created.
#' @param interactive Whether the created plot should be interactive.
#' 
#' @seealso [score_summary()]
#' 
#' @name score_summary_module
#' @export
score_summary_ui <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    type = "hidden", id = ns("wizard"),
    tabPanelBody(
      "interactive",
      div(
        id = "summary_waiter",
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

#' @name score_summary_module
#' @export
score_summary_server <- function(id, column, score_spec, interactive) {
  moduleServer(id, function(input, output, session) {
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
    
    waiter <- waiter::Waiter$new(
      id = "summary_waiter", color = "white",
      waiter::spin_three_bounce()
    )
    
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
    plot <- reactive({
      req(column(), score())
      waiter$show()
      
      data <- tibble::tibble(
        column = column(),
        score = score()
      )
      
      # Create the plot
      ggplot2::ggplot(data, ggplot2::aes(x = column, y = score)) +
        ggplot2::geom_line() +
        ggplot2::ylim(0,1) + # Make sure the y axis always has the same scale
        ggplot2::labs(x = "Column value", y = "Score") +
        ggplot2::ggtitle("Score distribution")
    })
    
    output$plot <- plotly::renderPlotly({
      req(plot(), !identical(interactive_value(), FALSE))
      p <- plotly::ggplotly(plot(), source = "summary") # Makes the plot interactive
      plotly::event_register(p, "plotly_afterplot")
      p
    }) %>%
      bindEvent(plot())
    
    observe({
      waiter$hide()
    }) %>%
      bindEvent({
        req(plot(), !identical(interactive_value(), FALSE))
        plotly::event_data("plotly_afterplot", source = "summary", 
                           priority = "event")
      })
    
    output$normal_plot <- renderPlot({
      req(plot(), identical(interactive_value(), FALSE))
      plot()
    }) %>%
      bindEvent(plot())
  })
}

#' View a summary of a selected stock
#' 
#' A shiny module that allows the user to see a tabular summary of a stock they
#' have selected.
#' 
#' @param id The namespace of the module.
#' @param stock The stock that has been selected.
#' 
#' @seealso [stock_summary()]
#' 
#' @name stock_summary_module
#' @export
stock_summary_ui <- function(id) {
  ns <- NS(id)
  reactable::reactableOutput(ns("summary"))
}

#' @name stock_summary_module
#' @export
stock_summary_server <- function(id, stock) {
  moduleServer(id, function(input, output, session) {
    stock_row <- reactive({
      req(stock())
      default_stock_data[default_stock_data$symbol == stock(),]
    })
    
    output$summary <- reactable::renderReactable({
      req(stock_row())
      reactable::reactable(
        stock_row(), sortable = FALSE, pagination = FALSE,
        columns <- list(
          description = reactable::colDef(
            minWidth = 1000
          )
        )
      )
    })
  })
}
