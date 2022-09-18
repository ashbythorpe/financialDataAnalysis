fDA_dependencies <- function(){
  htmltools::htmlDependency(
    "financialDataAnalysis-assets",
    version = utils::packageVersion("financialDataAnalysis"),
    package = "financialDataAnalysis",
    src = "inst/assets",
    script = "js/script.js",
    stylesheet = "css/style.css"
  ) %>%
    createWebDependency()
}
