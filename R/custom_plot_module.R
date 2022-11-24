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
#' @name custom_plot_module
#' @export
custom_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("type"), "Plot type", choices = c(
      "Line graph", "Scatter graph", "Histogram", "Smoothed line graph",
      "Area graph", "Hexagon heatmap"
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
      ),
      tabPanelBody(
        "smooth",
        smooth_graph_ui(ns("smooth"))
      ),
      tabPanelBody(
        "area",
        smooth_graph_ui(ns("area"))
      ),
      tabPanelBody(
        "hex",
        smooth_graph_ui(ns("hex"))
      )
    )
  )
}

#' @name custom_plot_module
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
        "Smoothed line graph" = "smooth",
        "Area graph" = "area", 
        "Hexagon heatmap" = "hex",
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
    smooth_args <- smooth_graph_server("smooth", data)
    area_args <- smooth_graph_server("area", data)
    hex_args <- smooth_graph_server("hex", data)
    
    args <- reactive({
      req(type())
      switch(
        type(),
        line = line_args(),
        scatter = scatter_args(),
        histogram = histogram_args(),
        smooth = smooth_args(),
        area = area_args(),
        hex = hex_args()
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
#' @name custom_plot_modules
#' @export
line_graph_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("x"), "X axis", choices = c("")),
    selectInput(ns("y"), "Y axis", choices = c("")),
    selectInput(ns("alpha"), "Alpha", choices = c("")),
    selectInput(ns("colour"), "Colour", choices = c("")),
    selectInput(ns("linewidth"), "Line width", choices = c("")),
    v_numeric_input(ns("opacity"), "Opacity", value = 1, min = 0),
    v_numeric_input(ns("size"), "Size", value = 1, min = 0)
  )
}

#' @name custom_plot_modules
#' @export
line_graph_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observe({
      df_names <- colnames(data())
      numeric_names <- df_names[purrr::map_lgl(data(), is.numeric)]
      updateSelectInput(session, "x", choices = df_names, 
                        selected = df_names[1])
      updateSelectInput(session, "y", choices = df_names, 
                        selected = df_names[1])
      updateSelectInput(session, "alpha", choices = c("None", df_names),
                        selected = "None")
      updateSelectInput(session, "colour", choices = c("None", df_names), 
                        selected = "None")
      updateSelectInput(session, "linewidth", choices = c("None", numeric_names),
                        selected = "None")
    }) %>%
      bindEvent(data())
    
    args <- reactive({
      x <- list(
        x = input$x,
        y = input$y,
        alpha = input$alpha,
        colour = input$colour,
        linewidth = input$linewidth,
        opacity = if(isTRUE(input$opacity < 0)) 0 else input$opacity,
        size = if(isTRUE(input$size < 0)) 0 else input$size
      )
      x[!purrr::map_lgl(x, identical, "None")]
    })
    
    args
  })
}

#' @name custom_plot_modules
#' @export
scatter_graph_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("x"), "X axis", choices = c("")),
    selectInput(ns("y"), "Y axis", choices = c("")),
    selectInput(ns("alpha"), "Alpha", choices = c("")),
    selectInput(ns("colour"), "Colour", choices = c("")),
    selectInput(ns("shape"), "Shape", choices = c("")),
    selectInput(ns("size"), "Size", choices = c("")),
    v_numeric_input(ns("opacity"), "Opacity", value = 1, min = 0),
    v_numeric_input(ns("pointsize"), "Point size", value = 1, min = 0),
    shinyWidgets::prettySwitch(ns("jitter"), "Jitter")
  )
}

#' @name custom_plot_modules
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
      updateSelectInput(session, "alpha", choices = c("None", df_names),
                        selected = "None")
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
        alpha = input$alpha,
        colour = input$colour,
        size = input$size,
        shape = input$shape,
        opacity = if(isTRUE(input$opacity < 0)) 0 else input$opacity,
        pointsize = if(isTRUE(input$pointsize < 0)) 0 else input$pointsize
      )
      x[!purrr::map_lgl(x, identical, "None")]
    })
    
    args
  })
}

#' @name custom_plot_modules
#' @export
histogram_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("x"), "X axis", choices = c("")),
    selectInput(ns("y"), "Y axis", choices = c("")),
    selectInput(ns("alpha"), "Alpha", choices = c("")),
    selectInput(ns("colour"), "Colour", choices = c("")),
    selectInput(ns("fill"), "Fill", choices = c("")),
    v_numeric_input(ns("opacity"), "Opacity", value = 1, min = 0),
    v_numeric_input(ns("bins"), "Bins", value = 30, min = 1, step = 1)
  )
}

#' @name custom_plot_modules
#' @export
histogram_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observe({
      df_names <- colnames(data())
      numeric_names <- df_names[purrr::map_lgl(data(), is.numeric)]
      updateSelectInput(session, "x", choices = c("None", df_names), 
                        selected = df_names[1])
      updateSelectInput(session, "y", choices = c("None", df_names), 
                        selected = "None")
      updateSelectInput(session, "alpha", choices = c("None", df_names),
                        selected = "None")
      updateSelectInput(session, "colour", choices = c("None", df_names), 
                        selected = "None")
      updateSelectInput(session, "fill", choices = c("None", df_names), 
                        selected = "None")
    }) %>%
      bindEvent(data())
    
    observe({
      df_names <- colnames(data())
      if(input$x == "None" && input$y == "None") {
        updateSelectInput(session, "y", selected = df_names[1])
      } else if(input$x != "None" && input$y != "None") {
        updateSelectInput(session, "y", selected = "None")
      }
    }) %>%
      bindEvent(input$x)
    
    observe({
      df_names <- colnames(data())
      if(input$y == "None" && input$x == "None") {
        updateSelectInput(session, "x", selected = df_names[1])
      } else if(input$y != "None" && input$x != "None") {
        updateSelectInput(session, "x", selected = "None")
      }
    }) %>%
      bindEvent(input$y)
    
    args <- reactive({
      x <- list(
        x = input$x,
        y = input$y,
        alpha = input$alpha,
        colour = input$colour,
        size = input$size,
        opacity = if(isTRUE(input$opacity < 0)) 0 else input$opacity,
        bins = if(isTRUE(input$bins < 1)) 1 else round(input$bins),
      )
      x[!purrr::map_lgl(x, identical, "None")]
    })
    
    args
  })
}

#' @name custom_plot_modules
#' @export
smooth_graph_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("x"), "X axis", choices = c("")),
    selectInput(ns("y"), "Y axis", choices = c("")),
    selectInput(ns("alpha"), "Alpha", choices = c("")),
    v_numeric_input(ns("opacity"), "Opacity", value = 1, min = 0),
    v_numeric_input(ns("span"), "Span", value = 0.75, min = 0)
  )
}

#' @name custom_plot_modules
#' @export
smooth_graph_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observe({
      df_names <- colnames(data())
      numeric_names <- df_names[purrr::map_lgl(data(), is.numeric)]
      updateSelectInput(session, "x", choices = df_names, 
                        selected = df_names[1])
      updateSelectInput(session, "y", choices = df_names, 
                        selected = df_names[1])
      updateSelectInput(session, "alpha", choices = c("None", df_names),
                        selected = "None")
    }) %>%
      bindEvent(data())
    
    args <- reactive({
      x <- list(
        x = input$x,
        y = input$y,
        alpha = input$alpha,
        opacity = if(isTRUE(input$opacity < 0)) 0 else input$opacity,
        span = if(isTRUE(input$span < 0)) 0 else input$span
      )
      x[!purrr::map_lgl(x, identical, "None")]
    })
    
    args
  })
}

#' @name custom_plot_modules
#' @export
area_graph_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("x"), "X axis", choices = c("")),
    selectInput(ns("y"), "Y axis", choices = c("")),
    selectInput(ns("alpha"), "Alpha", choices = c("")),
    selectInput(ns("colour"), "Colour", choices = c("")),
    selectInput(ns("fill"), "Fill", choices = c("")),
    selectInput(ns("linewidth"), "Line width", choices = c("")),
    v_numeric_input(ns("opacity"), "Opacity", value = 1, min = 0),
    v_numeric_input(ns("size"), "Size", value = 1, min = 0)
  )
}

#' @name custom_plot_modules
#' @export
area_graph_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observe({
      df_names <- colnames(data())
      numeric_names <- df_names[purrr::map_lgl(data(), is.numeric)]
      updateSelectInput(session, "x", choices = df_names, 
                        selected = df_names[1])
      updateSelectInput(session, "y", choices = df_names, 
                        selected = df_names[1])
      updateSelectInput(session, "alpha", choices = c("None", numeric_names),
                        selected = "None")
      updateSelectInput(session, "colour", choices = c("None", df_names), 
                        selected = "None")
      updateSelectInput(session, "fill", choices = c("None", df_names), 
                        selected = "None")
      updateSelectInput(session, "linewidth", choices = c("None", numeric_names),
                        selected = "None")
    }) %>%
      bindEvent(data())
    
    args <- reactive({
      x <- list(
        x = input$x,
        y = input$y,
        alpha = input$alpha,
        colour = input$colour,
        fill = input$fill,
        linewidth = input$linewidth,
        opacity = if(isTRUE(input$opacity < 0)) 0 else input$opacity,
        size = if(isTRUE(input$size < 0)) 0 else input$size
      )
      x[!purrr::map_lgl(x, identical, "None")]
    })
    
    args
  })
}

#' @name custom_plot_modules
#' @export
histogram_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("x"), "X axis", choices = c("")),
    selectInput(ns("y"), "Y axis", choices = c("")),
    selectInput(ns("alpha"), "Alpha", choices = c("")),
    selectInput(ns("colour"), "Colour", choices = c("")),
    selectInput(ns("fill"), "Fill", choices = c("")),
    v_numeric_input(ns("opacity"), "Opacity", value = 1, min = 0),
    v_numeric_input(ns("bins"), "Bins", value = 30, min = 1, step = 1)
  )
}

#' @name custom_plot_modules
#' @export
histogram_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observe({
      df_names <- colnames(data())
      numeric_names <- df_names[purrr::map_lgl(data(), is.numeric)]
      updateSelectInput(session, "x", choices = c("None", df_names), 
                        selected = df_names[1])
      updateSelectInput(session, "y", choices = c("None", df_names), 
                        selected = "None")
      updateSelectInput(session, "alpha", choices = c("None", df_names),
                        selected = "None")
      updateSelectInput(session, "colour", choices = c("None", df_names), 
                        selected = "None")
      updateSelectInput(session, "fill", choices = c("None", df_names), 
                        selected = "None")
    }) %>%
      bindEvent(data())
    
    observe({
      df_names <- colnames(data())
      if(input$x == "None" && input$y == "None") {
        updateSelectInput(session, "y", selected = df_names[1])
      } else if(input$x != "None" && input$y != "None") {
        updateSelectInput(session, "y", selected = "None")
      }
    }) %>%
      bindEvent(input$x)
    
    observe({
      df_names <- colnames(data())
      if(input$y == "None" && input$x == "None") {
        updateSelectInput(session, "x", selected = df_names[1])
      } else if(input$y != "None" && input$x != "None") {
        updateSelectInput(session, "x", selected = "None")
      }
    }) %>%
      bindEvent(input$y)
    
    args <- reactive({
      x <- list(
        x = input$x,
        y = input$y,
        alpha = input$alpha,
        colour = input$colour,
        size = input$size,
        opacity = if(isTRUE(input$opacity < 0)) 0 else input$opacity,
        bins = if(isTRUE(input$bins < 1)) 1 else round(input$bins),
      )
      x[!purrr::map_lgl(x, identical, "None")]
    })
    
    args
  })
}
