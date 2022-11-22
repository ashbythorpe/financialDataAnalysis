test_module <- function(ui, server, ...) {
  call <- rlang::call_match()
  ui_call <- call$ui
  server_call <- call$server
  if(is.call(ui_call)) {
    ui_call <- rlang::call_modify(ui_call, id = "id")
    ui <- rlang::eval_tidy(ui_call)
  } else {
    ui <- ui("id")
  }
  
  actual_ui <- tagList(dashboardPage(
    dashboardHeader(title = "Financial Data Analysis"),
    dashboardSidebar(sidebarMenu(
      menuItem("", tabName = "tab")
    )),
    dashboardBody(tabItems(
      tabItem("tab",
              ui)
    ))
  ), fDA_dependencies())
  
  actual_server <- function(input, output, session) {
    if(is.call(server_call)) {
      server_call <- rlang::call_modify(server_call, ..., id = "id")
      server <- rlang::eval_tidy(server_call)
    } else {
      server <- server("id", ...)
    }
    
    res <- server
    
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
