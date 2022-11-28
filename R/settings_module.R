#' Specify various site settings
#' 
#' A shiny module that contains the Settings page. Allows the user to input
#' various settings. At the moment, the only setting is one which allows them
#' to disable interactive plots.
#' 
#' @param id The namespace of the module.
#' 
#' @returns The user's inputted settings
#' 
#' @name settings_module
#' @export
settings_ui <- function(id) {
  ns <- NS(id)
  box(
    title = "Settings",
    shinyWidgets::prettySwitch(ns("interactive"), "Disable interactive plots") %>%
      add_info("interactive")
  )
}

#' @name settings_module
#' @export
settings_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    interactive <- reactive({
      req(input$interactive)
      !input$interactive
    })
    
    list(
      interactive = interactive
    )
  })
}
