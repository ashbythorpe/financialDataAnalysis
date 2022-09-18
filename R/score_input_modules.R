score_type_ui <- function(id){
  ns <- NS(id)
  selectInput(ns("score_type"), "Score type", choices = c(
    "Linear", "Peak", "Custom coordinates"
  ))
}

score_type_server <- function(id){
  moduleServer(id, function(input, output, session){
    score_type <- reactive({
      validate_score_type(input$score_type)
    })
    
    score_type
  })
}

universal_score_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("colname"), "Column", choices = c("")),
    textInput(ns("score_name"), "Score name", value = "Default"),
    numericInput(ns("weight"), "Weight", value = 1)
  )
}

universal_score_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    valid_colnames <- reactive({
      req(data())
      colnames(data())[purrr::map_lgl(data(), is.numeric)]
    })
    
    observe({
      updateSelectInput(session, "colname", choices = valid_colnames(), 
                        selected = valid_colnames()[1])
    })
    
    universal_row <- reactive({
      validate_universal_score(input$colname, input$score_name, input$weight)
    })
    
    universal_row
  })
}

linear_score_ui <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("lb"), "Lower bound", value = 0),
    numericInput(ns("ub"), "Upper bound", value = 0)
  )
}

linear_score_server <- function(id, column) {
  moduleServer(id, function(input, output, session) {
    minc <- reactive(min(column(), na.rm = T))
    maxc <- reactive(max(column(), na.rm = T))
    
    observe({
      updateNumericInput(session, "lb", value = minc())
      updateNumericInput(session, "ub", value = maxc())
    })
    
    linear_row <- reactive({
      validate_linear_score(input$lb, input$ub)
    })
  })
}

peak_score_ui <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("lb"), "Lower bound", value = 0),
    numericInput(ns("ub"), "Upper bound", value = 0),
    numericInput(ns("centre"), "Centre", value = 0),
    shinyWidgets::prettySwitch(ns("inverse"), "Inverse")
  )
}

peak_score_server <- function(id, column) {
  moduleServer(id, function(input, output, session) {
    minc <- reactive(min(column(), na.rm = T))
    maxc <- reactive(max(column(), na.rm = T))
    
    observe({
      updateNumericInput(session, "lb", value = minc())
      updateNumericInput(session, "ub", value = maxc())
      updateNumericInput(session, "centre", value = (minc() + maxc())/2)
    })
    
    peak_row <- reactive({
      validate_peak_score(input$lb, input$ub, input$centre, input$inverse)
    })
    
    peak_row
  })
}

exponential_score_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyWidgets::prettySwitch(ns("exponential"), "Exponential transformation"),
    conditionalPanel(
      "input.exponential", ns = ns,
      shinyWidgets::prettySwitch(ns("logarithmic"), "Logarithmic"),
      numericInput(ns("magnitude"), "Magnitude", value = 1)
    )
  )
}

exponential_score_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    exponential_row <- reactive({
      validate_exponential_transformation(
        input$exponential, input$logarithmic, input$magnitude
      )
    })
    
    exponential_row
  })
}
