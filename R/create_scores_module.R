create_scores_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(
      width = 6,
      box(
        title = "Define a score:",
        editing_ui(ns("editing")),
        score_type_ui(ns("score_type")),
        universal_score_ui(ns("universal_score")),
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
        scores_table_ui(ns("scores_table")),
      ),
      box(score_summary_ui(ns("score_summary")))
    )
  )
}

create_scores_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    values <- reactiveValues()
    
    values$scores <- scores_init
    values$editing <- NA
    
    score_type <- score_type_server("score_type")
    
    observe({
      updateTabsetPanel(session, "wizard", selected = score_type())
    })
    
    column <- reactive({
      data()[[universal_score()$colname]]
    })
    
    universal_score <- universal_score_server("universal_score", data)
    linear_score <- linear_score_server("linear_score", column)
    peak_score <- peak_score_server("peak_score", column)
    custom_score <- custom_score_server("custom_score", column)
    exponential_score <- exponential_score_server("exponential_score")
    
    score_args <- reactive({
      req(score_type())
      switch(score_type(),
        Linear = linear_score(),
        Peak = peak_score(),
        `Custom coordinates` = custom_score()
      )
    })
    
    final_row <- reactive({
      bind_validated_columns(score_type(), universal_score(), score_args(),
                             exponential_score())
    })
    
    observe({
      .f <- get_score_function(values$editing)
      values$scores <- .f(values$scores, final_row())
    }) %>%
      bindEvent(input$create_score)
    
    scores_values <- scores_table_server("scores_table", reactive(values$scores))
    editing <- reactive(scores_values$editing())
    deleting <- reactive(scores_values$deleting())
    
    observe({
      if(isTruthy(editing())){
        values$editing <- editing()
      }
    }) %>%
      bindEvent(editing())
    
    observe({
      if(isTruthy(deleting())){
        values$scores <- values$scores %>%
          dplyr::slice(-deleting())
      }
    }) %>%
      bindEvent(deleting())
    
    stop_editing <- editing_server("editing", reactive(values$editing),
                                   reactive(values$scores))
    
    observe({
      values$editing <- NA
    }) %>%
      bindEvent(stop_editing())
    
    score_summary_server("score_summary", data, final_row)
    
    reactive(values$scores)
  })
}

