#' Introduction to the application
#' 
#' Returns the home page of the application
#' 
#' @rdname home_module
#' @export
home_ui <- function(){
  tagList(
    box(
      p("Welcome to the Financial Data Analysis website!"),
    ),
    box(
      p("Rank data, forecast stock prices and create plots.")
    ),
    box(
      p("Contact me:"),
      p("Email: ashbythorpe@gmail.com")
    ),
    box(
      a("Source code", 
        href = "https://github.com/ashbythorpe/financialDataAnalysis")
    )
  )
}
