#' View the table of scores
#' 
#' A shiny module that allows the user to view, edit and delete created score
#' specifications.
#' 
#' @param id The namespace of the module.
#' @param scores The data frame of score specifications created by the user. See
#'   [scores_init].
#' 
#' @returns 
#' The server returns the edited and deleted scores.
#' 
#' @name scores_table_module
#' @export
scores_table_ui <- function(id) {
  ns <- NS(id)
  # Only show the table if there is at least one score
  # Otherwise, show some placeholder text
  tabsetPanel(
    id = ns("wizard"), type = "hidden",
    tabPanelBody("no_scores", p("Create a score")),
    tabPanelBody(
      "scores",
      div(
        class = "box_row",
        actionButton(ns("edit"), "Edit selected score"),
        actionButton(ns("delete"), "Delete selected score")
      ),
      reactable::reactableOutput(ns("scores"))
    )
  )
}

#' @name scores_table_module
#' @export
scores_table_server <- function(id, scores) {
  moduleServer(id, function(input, output, session) {
    # Update the UI to show the table if there are scores and vice versa
    observe({
      if(nrow(scores()) == 0){
        updateTabsetPanel(session, "wizard", selected = "no_scores")
      } else{
        updateTabsetPanel(session, "wizard", selected = "scores")
      }
    })
    
    # Create the table
    output$scores <- reactable::renderReactable({
      scores() %>%
        dplyr::select(score_name, score_type, colname) %>%
        dplyr::rename(
          `Score name` = "score_name",
          `Score type` = "score_type",
          Column = "colname"
        ) %>%
        reactable::reactable(sortable = FALSE, pagination = FALSE, 
                             selection = "multiple", onClick = "select",
                             highlight = TRUE)
    })
    
    # Get the currently selected row indexes
    selected <- reactive(reactable::getReactableState("scores", "selected"))
    
    # The first value of selected, updated when the edit button is clicked
    editing <- reactive({
      selected()[1]
    }) %>%
      bindEvent(input$edit)
    
    # selected, updated when the delete button is clicked
    deleting <- reactive(selected()) %>%
      bindEvent(input$delete)
    
    # Reset the row selection when the edit or delete buttons are clicked
    observe({
      reactable::updateReactable("scores", selected = NA)
    }) %>%
      bindEvent(input$edit, input$delete)
    
    # Return editing and deleting
    list(editing = editing,
         deleting = deleting)
  })
}
