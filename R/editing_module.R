editing_ui <- function(id) {
  ns <- NS(id)
  # Only display the text and button when the user is editing a score.
  tabsetPanel(
    id = ns("wizard"), type = "hidden",
    tabPanelBody("not_editing", NULL),
    tabPanelBody(
      "editing",
      div(
        class = "box_row",
        textOutput(ns("editing")),
        actionButton(ns("stop_editing"), "Stop editing", icon = icon("xmark"))
      )
    )
  )
}

editing_server <- function(id, editing, scores) {
  moduleServer(id, function(input, output, session) {
    # Show the text and button when the user is editing a score and vice versa
    observe({
      if(is.na(editing())){
        updateTabsetPanel(session, "wizard", "not_editing")
      } else{
        updateTabsetPanel(session, "wizard", "editing")
      }
    })
    
    # Tell the user the name of the score they are editing
    output$editing <- reactive({
      if(!is.na(editing()) && editing() <= nrow(scores())){
        score_name <- scores()$score_name[editing()]
        glue::glue_safe("Editing \"{score_name}\"")
      }
    })
    
    # Return the value corresponding to the stop_editing button
    reactive(input$stop_editing)
  })
}
