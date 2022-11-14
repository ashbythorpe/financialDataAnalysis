custom_plot_ui <- function(id) {
  tagList(
    selectInput("type", "Plot type", choices = c(
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
    
    line_args <- line_graph_server("line")
    scatter_args <- scatter_graph_server("scatter")
    histogram_args <- histogram_server("histogram")
    
    args <- reactive({
      req(type())
      switch(
        type(),
        line = line_args(),
        scatter = scatter_args(),
        histogram = histogram_args()
      )
    })
    
    args
  })
}

line_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("x"), "X axis", choices = c("")),
    selectInput(ns("y"), "Y axis", choices = c("")),
    selectInput(ns("colour", "Colour", choices = c("")))
  )
}

line_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observe({
      df_names <- colnames(data())
      updateSelectInput(session, "x", choices = df_names, 
                        selected = df_names[1])
      updateSelectInput(session, "y", choices = df_names, 
                        selected = df_names[1])
      updateSelectInput(session, "colour", choices = df_names, 
                        selected = df_names[1])
    }) %>%
      bindEvent(data())
    
    args <- reactive({
      list(
        x = input$x,
        y = input$y,
        colour = input$colour
      )
    })
    
    args
  })
}

scatter_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("x"), "X axis", choices = c("")),
    selectInput(ns("y"), "Y axis", choices = c("")),
    selectInput(ns("colour", "Colour", choices = c(""))),
    selectInput(ns("size", "Size", choices = c(""))),
    selectInput(ns("shape", "Shape", choices = c("")))
  )
}

line_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observe({
      df_names <- colnames(data())
      numeric_names <- df_names[purrr::map_lgl(data(), is.numeric)]
      shape_names <- df_names[purrr::map_lgl(data() ~ {
        length(unique(.)) <= 6
      })]
      updateSelectInput(session, "x", choices = df_names, 
                        selected = df_names[1])
      updateSelectInput(session, "y", choices = df_names, 
                        selected = df_names[1])
      updateSelectInput(session, "colour", choices = df_names, 
                        selected = df_names[1])
      updateSelectInput(session, "size", choices = numeric_names,
                        selected = numeric_names[1])
    }) %>%
      bindEvent(data())
    
    args <- reactive({
      list(
        x = input$x,
        y = input$y,
        colour = input$colour,
        size = input$size,
        shape - input$shape
      )
    })
    
    args
  })
}

histogram_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("x"), "X axis", choices = c("")),
    selectInput(ns("colour", "Colour", choices = c(""))),
    selectInput(ns("size", "Size", choices = c(""))),
  )
}

line_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observe({
      df_names <- colnames(data())
      numeric_names <- df_names[purrr::map_lgl(data(), is.numeric)]
      updateSelectInput(session, "x", choices = df_names, 
                        selected = df_names[1])
      updateSelectInput(session, "y", choices = df_names, 
                        selected = df_names[1])
      updateSelectInput(session, "colour", choices = df_names, 
                        selected = df_names[1])
      updateSelectInput(session, "size", choices = numeric_names,
                        selected = numeric_names[1])
    }) %>%
      bindEvent(data())
    
    args <- reactive({
      list(
        x = input$x,
        y = input$y,
        colour = input$colour,
        size = input$size
      )
    })
    
    args
  })
}
