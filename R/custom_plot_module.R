#' Input the arguments for a custom plot
#' 
#' A shiny module that allows the user to input the arguments in order to create
#' a custom plot.
#' 
#' @param id The namespace of the module.
#' @param custom Whether the user wants to make a custom plot.
#' @param data The data that is being plotted.
#' 
#' @returns 
#' The server returns a list of validated arguments, along with the inputted
#' plot type.
#' 
#' @seealso 
#' * [custom_plot()]
#' * The modules for the different custom plot types: [custom_plot_modules]
#' 
#' @rdname custom_plot_module
#' @export
custom_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("type"), "Plot type", choices = c(
      "Line graph", "Scatter graph", "Histogram"
    )),
    tabsetPanel(
      id = ns("wizard"), type = "hidden",
      tabPanelBody(
        "line",
        line_graph_ui(ns("line"))
      ),
      tabPanelBody(
        "scatter",
        scatter_graph_ui(ns("scatter"))
      ),
      tabPanelBody(
        "histogram",
        histogram_ui(ns("histogram"))
      )
    )
  )
}

#' @rdname custom_plot_module
#' @export
custom_plot_server <- function(id, custom, data) {
  moduleServer(id, function(input, output, session) {
    type <- reactive({
      req(input$type, custom())
      switch(
        input$type,
        "Line graph" = "line",
        "Scatter graph" = "scatter",
        "Histogram" = "histogram",
        NULL
      )
    })
    
    observe({
      updateTabsetPanel(session, "wizard", type())
    }) %>%
      bindEvent(type())
    
    line_args <- line_graph_server("line", data)
    scatter_args <- scatter_graph_server("scatter", data)
    histogram_args <- histogram_server("histogram", data)
    
    args <- reactive({
      req(type())
      switch(
        type(),
        line = line_args(),
        scatter = scatter_args(),
        histogram = histogram_args()
      )
    })
    
    list(
      type = type,
      args = args
    )
  })
}

#' Input arguments for different custom plots
#' 
#' A set of shiny modules that allow the user to input a set of arguments
#' depending on the plot type.
#' 
#' @param id The namespace of the module.
#' @param data The data that is being plotted
#' 
#' @returns
#' Each module's server returns a list of validated arguments.
#' 
#' @seealso [custom_plot_module]
#' 
#' @rdname custom_plot_modules
#' @export
line_graph_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("x"), "X axis", choices = c("")),
    selectInput(ns("y"), "Y axis", choices = c("")),
    selectInput(ns("colour"), "Colour", choices = c(""))
  )
}

#' @rdname custom_plot_modules
#' @export
line_graph_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observe({
      df_names <- colnames(data())
      updateSelectInput(session, "x", choices = df_names, 
                        selected = df_names[1])
      updateSelectInput(session, "y", choices = df_names, 
                        selected = df_names[1])
      updateSelectInput(session, "colour", choices = c("None", df_names), 
                        selected = "None")
    }) %>%
      bindEvent(data())
    
    args <- reactive({
      x <- list(
        x = input$x,
        y = input$y,
        colour = input$colour
      )
      x[!purrr::map_lgl(x, identical, "None")]
    })
    
    args
  })
}

#' @rdname custom_plot_modules
#' @export
scatter_graph_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("x"), "X axis", choices = c("")),
    selectInput(ns("y"), "Y axis", choices = c("")),
    selectInput(ns("colour"), "Colour", choices = c("")),
    selectInput(ns("size"), "Size", choices = c("")),
    selectInput(ns("shape"), "Shape", choices = c(""))
  )
}

#' @rdname custom_plot_modules
#' @export
scatter_graph_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observe({
      df_names <- colnames(data())
      numeric_names <- df_names[purrr::map_lgl(data(), is.numeric)]
      shape_names <- df_names[purrr::map_lgl(data(), ~ {
        length(unique(.)) <= 6
      })]
      updateSelectInput(session, "x", choices = df_names, 
                        selected = df_names[1])
      updateSelectInput(session, "y", choices = df_names, 
                        selected = df_names[1])
      updateSelectInput(session, "colour", choices = c("None", df_names), 
                        selected = "None")
      updateSelectInput(session, "size", choices = c("None", numeric_names),
                        selected = "None")
      updateSelectInput(session, "shape", choices = c("None", shape_names),
                        selected = "None")
    }) %>%
      bindEvent(data())
    
    args <- reactive({
      x <- list(
        x = input$x,
        y = input$y,
        colour = input$colour,
        size = input$size,
        shape = input$shape
      )
      x[!purrr::map_lgl(x, identical, "None")]
    })
    
    args
  })
}

#' @rdname custom_plot_modules
#' @export
histogram_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("x"), "X axis", choices = c("")),
    selectInput(ns("colour"), "Colour", choices = c("")),
    selectInput(ns("size"), "Size", choices = c("")),
  )
}

#' @rdname custom_plot_modules
#' @export
histogram_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observe({
      df_names <- colnames(data())
      numeric_names <- df_names[purrr::map_lgl(data(), is.numeric)]
      updateSelectInput(session, "x", choices = df_names, 
                        selected = df_names[1])
      updateSelectInput(session, "colour", choices = c("None", df_names), 
                        selected = "None")
      updateSelectInput(session, "size", choices = c("None", numeric_names),
                        selected = "None")
    }) %>%
      bindEvent(data())
    
    args <- reactive({
      x <- list(
        x = input$x,
        colour = input$colour,
        size = input$size
      )
      x[!purrr::map_lgl(x, identical, "None")]
    })
    
    args
  })
}
