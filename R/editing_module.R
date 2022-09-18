editing_ui <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    id = ns("wizard"), type = "hidden",
    tabPanelBody("not_editing", NULL),
    tabPanelBody(
      "editing",
      fluidRow(
        textOutput(ns("editing")),
        actionButton(ns("stop_editing"), "Stop editing")
      )
    )
  )
}

editing_server <- function(id, editing, scores) {
  moduleServer(id, function(input, output, session) {
    observe({
      if(is.na(editing())){
        updateTabsetPanel(session, "wizard", "not_editing")
      } else{
        updateTabsetPanel(session, "wizard", "editing")
      }
    })
    
    output$editing <- reactive({
      if(!is.na(editing()) && editing() <= nrow(scores())){
        score_name <- scores()$score_name[editing()]
        glue::glue_safe("Editing \"{score_name}\"")
      }
    })
    
    reactive(input$stop_editing)
  })
}
