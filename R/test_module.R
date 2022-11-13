test_module <- function(ui, server, ...) {
  force(server)
  actual_ui <- tagList(dashboardPage(
    dashboardHeader(title = "Financial Data Analysis"),
    dashboardSidebar(sidebarMenu(
      menuItem("", tabName = "tab")
    )),
    dashboardBody(tabItems(
      tabItem("tab",
              ui("id"))
    ))
  ), fDA_dependencies())
  
  actual_server <- function(input, output, session) {
    res <- server("id", ...)
    
    observe({
      if(is.reactive(res)) {
        cat("Module server output:\n")
        print(res())
      }
    })
  }
  
  withr::with_options(list(shiny.reactlog = T), {
    shinyApp(actual_ui, actual_server)
  })
}
