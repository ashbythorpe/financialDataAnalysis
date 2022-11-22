#' Create score specifications
#' 
#' A shiny module that contains the Create Scores page, allowing the user to
#' create a set of score specifications. These are then automatically applied
#' to the data.
#' 
#' @param id The namespace of the module.
#' @param data The data to score.
#' 
#' @returns 
#' The server returns the table of score specifications that the user has 
#' created.
#' 
#' @name create_scores_module
#' @export
create_scores_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(
      width = 6,
      box(
        width = NULL, # Because the box is within a column
        title = "Define a score",
        editing_ui(ns("editing")),
        score_type_ui(ns("score_type")),
        universal_score_ui(ns("universal_score")),
        # Changes depending on the score type
        tabsetPanel(
          id = ns("wizard"), type = "hidden",
          tabPanelBody("Linear", linear_score_ui(ns("linear_score"))),
          tabPanelBody("Peak", peak_score_ui(ns("peak_score"))),
          tabPanelBody("Custom coordinates", custom_score_ui(ns("custom_score")))
        ),
        exponential_score_ui(ns("exponential_score")),
        actionButton(ns("create_score"), "Create score")
      )
    ),
    column(
      width = 6,
      box(
        title = "Scores",
        width = NULL,
        scores_table_ui(ns("scores_table")),
      ),
      box(
        width = NULL,
        score_summary_ui(ns("score_summary"))
      )
    )
  )
}

#' @name create_scores_module
#' @export
create_scores_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    values <- reactiveValues()
    
    # When this value is changed, all inputs will be reset.
    values$trigger_reset <- FALSE
    
    reset <- reactive(values$trigger_reset)
    
    # Define initial scores and editing values when data is initialised or 
    # changed
    observe({
      values$scores <- scores_init
      values$editing <- NA
      values$editing_row <- NULL
    }) %>%
      bindEvent(data())
    
    # Reset inputs when data is changed, not when it is initiated
    observe({
      values$trigger_reset <- isolate(!values$trigger_reset)
    }, priority = 100) %>%
      bindEvent(data(), ignoreInit = TRUE)
    
    # Get the score that is currently being edited
    editing_row <- reactive({
      if(!is.na(values$editing) && values$editing <= nrow(values$scores)) {
        values$scores[values$editing,]
      } else {
        NULL
      }
    }) %>%
      bindEvent(values$editing, ignoreNULL = F)
    
    # Get and validate the score type
    score_type <- score_type_server("score_type", reset, 
                                    editing_row = editing_row)
    
    # Update the tabsetPanel to show the options relating to the score type
    observe({
      if(!is.null(score_type())) {
        updateTabsetPanel(session, "wizard", selected = score_type())
      }
    })
    
    # Get and the score name, column name and weight.
    # This is not yet validated
    universal_score_vals <- universal_score_server("universal_score", data, reset,
                                              editing_row = editing_row)
    
    # Get the column from the specified column name
    column <- reactive({
      column <- data()[[universal_score_vals$colname()]]
      req(column)
      column
    })
    
    # Validate the universal score
    universal_score <- reactive({
      validate_universal_score(
        colname = universal_score_vals$colname(),
        score_name = universal_score_vals$score_name(),
        weight = universal_score_vals$weight()
      )
    })
    
    # Get and validate the rest of the score arguments
    linear_score <- linear_score_server("linear_score", column, reset,
                                        editing_row = editing_row)
    peak_score <- peak_score_server("peak_score", column, reset,
                                    editing_row = editing_row)
    custom_score <- custom_score_server("custom_score", column, reset,
                                        editing_row = editing_row)
    exponential_score <- exponential_score_server("exponential_score", reset,
                                                  editing_row = editing_row)
    
    # Get the arguments corresponding to the score type
    score_args <- reactive({
      req(score_type())
      switch(score_type(),
        Linear = linear_score(),
        Peak = peak_score(),
        `Custom coordinates` = custom_score()
      )
    })
    
    # Create the final score row (or NULL if any arguments are invalid)
    final_row <- reactive({
      bind_validated_columns(score_type(), universal_score(), score_args(),
                             exponential_score())
    })
    
    # Add the score to the scores table
    observe({
      # Only add the score if it is not invalid
      if(!is.null(final_row())) {
        .f <- get_score_function(values$editing)
        values$scores <- .f(values$scores, final_row()) %>%
          replace_score_names()
        # Make sure editing is reset if the user was editing a score
        values$editing <- NA
        # Trigger all inputs to reset
        values$trigger_reset <- !values$trigger_reset
      }
    }) %>%
      bindEvent(input$create_score)
    # Only create the score when the button is clicked
    
    # Display the table of scores, and allow the user to edit and delete scores
    scores_values <- scores_table_server("scores_table", reactive(values$scores))
    editing <- reactive(scores_values$editing())
    deleting <- reactive(scores_values$deleting())
    
    # If the user has specified a score to edit, change values$editing to
    # reflect this
    observe({
      if(isTruthy(editing())){
        values$editing <- editing()
      }
    }) %>%
      bindEvent(editing())
    
    # If the user has specified scores to delete, delete them.
    observe({
      if(isTruthy(deleting())) {
        if(!is.na(values$editing) && values$editing %in% deleting()) {
          # Stop editing a score if it is deleted
          values$editing <- NA
          values$trigger_reset <- !values$trigger_reset
        } else {
          # Make sure the correct row is still being edited
          values$editing <- 
            values$editing - length(which(deleting() <= values$editing))
        }
        # Remove the scores
        values$scores <- delete_scores(values$scores, deleting())
      }
    }) %>%
      bindEvent(deleting())
    
    # Display the score that is currently being edited
    # Allow the user to stop editing a score
    stop_editing <- editing_server("editing", reactive(values$editing),
                                   reactive(values$scores))
    
    # If the user has specified to stop editing a score, reset values$editing
    # and reset all inputs.
    observe({
      values$editing <- NA
      values$trigger_reset <- !values$trigger_reset
    }) %>%
      bindEvent(stop_editing())
    
    # Create a graphical visualisation of the score being created currently
    score_summary_server("score_summary", column, final_row)
    
    # Return the table of scores
    reactive(values$scores)
  })
}

