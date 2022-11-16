score_performance_ui <- function(id) {
  ns <- NS(id)
  selectInput(ns("colname"), label = "Column to compare against", 
              choices = c(""))
}

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
