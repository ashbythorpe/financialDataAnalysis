#' Dependencies
#' 
#' Get a list of HTML dependencies to be embedded in the app. This includes the
#' internal CSS and JavaScript files specific to this app, along with any other
#' external HTML dependencies.
#' 
#' @returns A collection of HTML tags.
#' 
#' @export
fDA_dependencies <- function(){
  tagList(
    htmltools::htmlDependency(
      "financialDataAnalysis-assets",
      version = utils::packageVersion("financialDataAnalysis"),
      package = "financialDataAnalysis",
      src = "inst/assets",
      script = "js/script.js",
      stylesheet = "css/style.css"
    ),
    htmltools::htmlDependency(
      "underscore",
      version = "1.13.6",
      src = c(href = "https://cdnjs.cloudflare.com/ajax/libs"),
      script = "underscore.js/1.13.6/underscore-umd-min.js"
    ),
    shinyjs::useShinyjs()
  )
}

