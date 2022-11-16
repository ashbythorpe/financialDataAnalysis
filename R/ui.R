# ui <- tagList(dashboardPage(
#   dashboardHeader(title = "Financial Data Analysis"),
#   dashboardSidebar(sidebarMenu(
#     menuItem("Home", tabName = "home"),
#     menuItem("Input Data", tabName = "data_input"),
#     menuItem("Create scores", tabName = "create_scores")
#   )),
#   dashboardBody(tabItems(
#     tabItem("home",
#             home_ui()),
#     tabItem("data_input",
#             data_input_ui("data_input")),
#     tabItem("create_scores",
#             create_scores_ui("create_scores"))
#   )),
# ), fDA_dependencies())


ui <- function() {
  tagList(dashboardPage(
    dashboardHeader(title = span("Financial Data Analysis", 
                                 style = "font-size: 18px")),
    dashboardSidebar(sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("house")),
      menuItem("Input Data", tabName = "data_input", icon = icon("upload")),
      menuItem("Create scores", tabName = "create_scores", 
               icon = icon("ranking-star")),
      menuItem("View data", tabName = "view_data", icon = icon("table")),
      menuItem("Forecast prices", tabName = "forecast_price", 
               icon = icon("chart-line")),
      menuItem("Plot data", tabName = "plot_data", icon = icon("chart-simple"))
    )),
    dashboardBody(tabItems(
      tabItem("home",
              home_ui()),
      tabItem("data_input",
              data_input_ui("data_input")),
      tabItem("create_scores",
              create_scores_ui("create_scores")),
      tabItem("view_data",
              view_data_ui("view_data")),
      tabItem("forecast_price",
              forecast_price_ui("forecast_price")),
      tabItem("plot_data",
              plot_data_ui("plot_data"))
    )),
  ), fDA_dependencies())
}
