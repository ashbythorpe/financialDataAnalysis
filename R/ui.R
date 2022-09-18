#' @include utils-pipe.R
#' @include utils.R
ui <- tagList(dashboardPage(
  dashboardHeader(title = "Financial Data Analysis"),
  dashboardSidebar(sidebarMenu(
    menuItem("Home", tabName = "home"),
    menuItem("Input Data", tabName = "data_input"),
    menuItem("Create scores", tabName = "create_scores")
  )),
  dashboardBody(tabItems(
    tabItem("home",
            home_ui()),
    tabItem("data_input",
            data_input_ui("data_input")),
    tabItem("create_scores",
            create_scores_ui("create_scores"))
  )),
), fDA_dependencies())

