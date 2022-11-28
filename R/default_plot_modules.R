#' Input a column to generate a score performance plot
#' 
#' A shiny module that allows the user to select a column to use for the score
#' performance plot.
#' 
#' @param id The namespace of the module.
#' @param data The data to plot.
#' @param scores The data frame of score specifications created by the user. See
#'   [scores_init].
#' 
#' @returns 
#' The server returns the selected column name.
#' 
#' @seealso [score_performance()]
#' 
#' @name score_performance_module   
#' @export
score_performance_ui <- function(id) {
  ns <- NS(id)
  selectInput(ns("colname"), label = "Column to compare against", 
              choices = c(""))
}

#' @name score_performance_module   
#' @export
score_performance_server <- function(id, data, scores) {
  moduleServer(id, function(input, output, session) {
    observe({
      valid_names <- colnames(data())[
        !colnames(data()) %in% c(scores()$score_name, "final_score")
      ]
      updateSelectInput(session, "colname", choices = valid_names, 
                        selected = valid_names[1])
    })
    
    reactive(input$colname)
  })
}

#' Decide whether to show text in a correlation heatmap plot
#' 
#' A shiny module that allows the user to decide the value of the `show_text`
#' argument to [correlation_plot()].
#' 
#' @param id The namespace of the module.
#' 
#' @returns The server returns the user's input.
#' 
#' @seealso [correlation_plot()]
#' 
#' @name correlation_plot_module
#' @export
correlation_plot_ui <- function(id) {
  ns <- NS(id)
  shinyWidgets::prettySwitch(ns("show_text"), "Show numbers as text") %>%
    add_info("show_text")
}

#' @name correlation_plot_module
#' @export
correlation_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    show_text <- reactive({
      req(is.logical(input$show_text))
      input$show_text
    })
    
    show_text
  })
}
