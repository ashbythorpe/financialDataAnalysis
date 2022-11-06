score_type_ui <- function(id){
  ns <- NS(id)
  selectInput(ns("score_type"), "Score type", choices = c(
    "Linear", "Peak", "Custom coordinates"
  ))
}

score_type_server <- function(id, reset, editing_row){
  moduleServer(id, function(input, output, session){
    # Reset the score type when reset() changes
    observe({
      updateSelectInput(session, "score_type", selected = "Linear")
    }) %>%
      bindEvent(reset(), ignoreInit = T) # No reason to update initially
    
    # Change the score type to the value of the score currently being edited
    observe({
      updateSelectInput(session, "score_type", selected = editing_row()$score_type)
    }) %>%
      bindEvent(editing_row(), ignoreInit = T)
    
    # Validate the inputted score type
    score_type <- reactive({
      validate_score_type(input$score_type)
    })
    
    # Return the validated score type
    score_type 
  })
}

universal_score_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("colname"), "Column", choices = c("")),
    col_summary_ui(ns("col_summary")),
    textInput(ns("score_name"), "Score name", value = "Default"),
    numericInput(ns("weight"), "Weight", value = 1)
  )
}

universal_score_server <- function(id, data, reset, editing_row){
  moduleServer(id, function(input, output, session){
    # Only the numeric columns should be considered scorable
    valid_colnames <- reactive({
      colnames(data())[purrr::map_lgl(data(), is.numeric)]
    })
    
    # Only allow the user to select scorable columns
    observe({
      updateSelectInput(session, "colname", choices = valid_colnames(), 
                        selected = valid_colnames()[1])
    })
    
    # Get the specified column
    column <- reactive({
      data()[[input$colname]]
    })
    
    # Create a summary of the specified column
    col_summary_server("col_summary", column)
    
    # Reset all inputs when reset() changes
    observe({
      updateSelectInput(session, "colname", selected = valid_colnames()[1])
      updateTextInput(session, "score_name", value = "Default")
      updateNumericInput(session, "weight", value = 1)
    }) %>%
      bindEvent(reset(), ignoreInit = TRUE)
    
    # Change all values to the values of the score currently being edited
    observe({
      updateSelectInput(session, "colname", selected = editing_row()$colname)
      updateTextInput(session, "score_name", value = editing_row()$score_name)
      updateNumericInput(session, "weight", value = editing_row()$weight)
    }) %>%
      bindEvent(editing_row(), ignoreInit = T)
    
    # Validate the column name, score name and weight
    universal_row <- reactive({
      validate_universal_score(input$colname, input$score_name, input$weight)
    })
    
    # Return the validated inputs
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

linear_score_server <- function(id, column, reset, editing_row) {
  moduleServer(id, function(input, output, session) {
    # Get the minimum and maximum of the column
    minc <- reactive(min(column(), na.rm = T))
    maxc <- reactive(max(column(), na.rm = T))
    
    # Update the lower bound to the minimum, and the upper bound to the maximum.
    # This will create a 'default' score.
    observe({
      updateNumericInput(session, "lb", value = minc())
      updateNumericInput(session, "ub", value = maxc())
    })
    
    # Reset all inputs when reset() changes
    observe({
      updateNumericInput(session, "lb", value = minc())
      updateNumericInput(session, "ub", value = maxc())
    }) %>%
      bindEvent(reset(), ignoreInit = TRUE)
    
    # Change all values to the values of the score currently being edited
    observe({
      # Stop these values from being updated unnecessarily
      if(identical(editing_row()$score_type, "Linear")) {
        updateNumericInput(session, "lb", value = editing_row()$lb)
        updateNumericInput(session, "ub", value = editing_row()$ub)
      }
    }) %>%
      bindEvent(editing_row(), ignoreInit = T)
    
    # Validate the lower bound and upper bound
    linear_row <- reactive({
      validate_linear_score(input$lb, input$ub)
    })
    
    # Return the validated inputs
    linear_row
  })
}

peak_score_ui <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("lb"), "Lower bound", value = 0),
    numericInput(ns("ub"), "Upper bound", value = 0),
    numericInput(ns("centre"), "Centre", value = 0),
    shinyWidgets::prettySwitch(ns("inverse"), "Inverse") # Defaults to FALSE
  )
}

peak_score_server <- function(id, column, reset, editing_row) {
  moduleServer(id, function(input, output, session) {
    # Get the minimum and maximum of the column
    minc <- reactive(min(column(), na.rm = T))
    maxc <- reactive(max(column(), na.rm = T))
    
    # Update the lower bound to the minimum, the upper bound to the maximum and
    # the centre to the middle of these two values.
    # Creates a 'default' score.
    observe({
      updateNumericInput(session, "lb", value = minc())
      updateNumericInput(session, "ub", value = maxc())
      updateNumericInput(session, "centre", value = (minc() + maxc())/2)
    })
    
    # Reset all inputs when reset() changes
    observe({
      updateNumericInput(session, "lb", value = minc())
      updateNumericInput(session, "ub", value = maxc())
      updateNumericInput(session, "centre", value = (minc() + maxc())/2)
    }) %>%
      bindEvent(reset(), ignoreInit = TRUE)
    
    # Change all values to the values of the score currently being edited
    observe({
      if(identical(editing_row()$score_type, "Peak")) {
        updateNumericInput(session, "lb", value = editing_row()$lb)
        updateNumericInput(session, "ub", value = editing_row()$ub)
        updateNumericInput(session, "centre", value = editing_row()$centre)
      }
    }) %>%
      bindEvent(editing_row(), ignoreInit = T)
    
    # Perform client side validation to stop the user inputting an invalid
    # value for input$centre
    observe({
      if(input$lb <= input$ub) {
        if(input$centre < input$lb) {
          updateNumericInput(session, "centre", value = input$lb)
        } else if(input$centre > input$ub) {
          updateNumericInput(session, "centre", value = input$ub)
        }
      } else {
        if(input$centre < input$ub) {
          updateNumericInput(session, "centre", value = input$ub)
        } else if(input$centre > input$lb) {
          updateNumericInput(session, "centre", value = input$lb)
        }
      }
    }) %>%
      debounce(1000) # Wait a second after the user has finished typing
    
    # Validate the lower bound, upper bound, centre and inverse
    peak_row <- reactive({
      validate_peak_score(input$lb, input$ub, input$centre, input$inverse)
    })
    
    # Return the validated inputs
    peak_row
  })
}

exponential_score_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyWidgets::prettySwitch(ns("exponential"), "Exponential transformation"),
    # Show the rest of the inputs if exponential is TRUE
    conditionalPanel(
      "input.exponential", ns = ns,
      shinyWidgets::prettySwitch(ns("logarithmic"), "Logarithmic"),
      numericInput(ns("magnitude"), "Magnitude", value = 1)
    )
  )
}

exponential_score_server <- function(id, reset, editing_row) {
  moduleServer(id, function(input, output, session) {
    # Reset all inputs when reset() changes
    observe({
      shinyWidgets::updatePrettySwitch(session, "exponential", value = FALSE)
      shinyWidgets::updatePrettySwitch(session, "logarithmic", value = FALSE)
      updateNumericInput(session, "magnitude", value = 1)
    }) %>%
      bindEvent(reset(), ignoreInit = TRUE)
    
    # Change all values to the values of the score currently being edited
    observe({
      shinyWidgets::updatePrettySwitch(session, "exponential", 
                                       value = editing_row()$exponential)
      # Stop unnecessary updating
      if(editing_row()$exponential) {
        shinyWidgets::updatePrettySwitch(session, "logarithmic", 
                                         value = editing_row()$logarithmic)
        updateNumericInput(session, "magnitude", value = editing_row()$magnitude)
      }
    }) %>%
      bindEvent(editing_row(), ignoreInit = T)
    
    # Validate exponential, logarithmic and magnitude
    exponential_row <- reactive({
      validate_exponential_transformation(
        input$exponential, input$logarithmic, input$magnitude
      )
    })
    
    # Return the validated inputs
    exponential_row
  })
}
