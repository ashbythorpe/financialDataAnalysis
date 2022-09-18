scores_table_ui <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    id = ns("wizard"), type = "hidden",
    tabPanelBody("no_scores", p("Create a score below")),
    tabPanelBody(
      "scores",
      fluidRow(
        actionButton(ns("edit"), "Edit selected score"),
        actionButton(ns("delete"), "Delete selected score")
      ),
      DT::DTOutput(ns("scores"))
    )
  )
}

scores_table_server <- function(id, scores) {
  moduleServer(id, function(input, output, session) {
    observe({
      if(nrow(scores()) == 0){
        updateTabsetPanel(session, "wizard", selected = "no_scores")
      } else{
        updateTabsetPanel(session, "wizard", selected = "scores")
      }
    })
    
    output$scores <- DT::renderDT({
      scores() %>%
        dplyr::select(score_name, score_type, colname) %>%
        dplyr::rename(Column = "colname")
    })
    
    editing <- reactive(input$scores_rows_selected[1]) %>%
      bindEvent(input$edit)
    
    deleting <- reactive(input$scores_rows_selected) %>%
      bindEvent(input$deleting)
    
    list(editing = editing,
         deleting = deleting)
  })
}
