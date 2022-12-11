#' Run the 'financialDataAnalysis' app
#'
#' Run the project as a Shiny application. The function calls
#' [shiny::shinyApp()] on the UI and server functions.
#'
#' @param ... Passed into [shiny::shinyApp()]
#'
#' @returns An object that represents the app.
#'
#' @seealso [ui()] [server()]
#'
#' @examples
#' \dontrun{
#' financialDataAnalysis()
#' }
#'
#' @import shiny
#' @import shinydashboard
#'
#' @export
financialDataAnalysis <- function(...) {
  shinyApp(ui, server, ...)
}
