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
      "popper",
      version = "2.11.6",
      src = c(href = "https://unpkg.com/@popperjs/"),
      script = "core@2/dist/umd/popper.min.js"
    ),
    htmltools::htmlDependency(
      "tippy",
      version = "6.3.7",
      src = c(href = "https://unpkg.com/tippy.js@6/"),
      script = "dist/tippy-bundle.umd.js"
    ),
    htmltools::htmlDependency(
      "financialDataAnalysis-assets",
      version = utils::packageVersion("financialDataAnalysis"),
      package = "financialDataAnalysis",
      src = "inst/assets",
      script = "js/script.js",
      stylesheet = "css/style.css"
    ),
    waiter::useWaiter(),
    waiter::useHostess()
  )
}

