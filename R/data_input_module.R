data_input_ui <- function(id){
  ns <- NS(id)
  accepted_files <- c(
    ".csv", ".xls", ".xlsx", ".xlsm", ".xltx", ".xltm"
  )
  
  box(title = "Upload data",
    fileInput(ns("files"), "Upload files:", multiple = T, 
              accept = accepted_files) %>%
      format_file_input(ns),
    textOutput(ns("fatal")) %>%
      tagAppendAttributes(class = "error"),
    textOutput(ns("nonfatal")) %>%
      tagAppendAttributes(class = "warning"),
    shinyWidgets::prettySwitch(ns("combine"),
                               label = "Combine files with default data?")
  )
}

data_input_server <- function(id){
  moduleServer(id, function(input, output, session){
    dfs <- reactive({
      if(is.null(input$files) || is.null(input$combine)) {
        NULL
      } else {
        read_files(input$files$datapath)
      }
    }) %>%
      bindEvent(input$files, ignoreNULL = FALSE)
    
    observe({
      shinyjs::reset("files")
    }) %>%
      bindEvent(input$reset)
    
    transformed_df <- reactive({
      if(is.null(dfs())) {
        NULL
      } else {
        dfs() %>%
          combine_if_multiple() %>%
          combine_if_specified(default_stock_data, input$combine) %>%
          transform_df(default_stock_data)
      }
    })
    
    error <- reactive({
      
      if(is.null(dfs()) || is.null(transformed_df())) {
        list(fatal = "", nonfatal = "")
      } else {
        get_error(input$files$datapath, dfs(), transformed_df())
      }
    })
    
    output$fatal <- reactive({
      error()$fatal
    })
    
    output$nonfatal <- reactive({
      error()$nonfatal
    })
    
    final_df <- reactive({
      if(is.null(transformed_df())) {
        default_stock_data
      } else {
        validate_df(transformed_df(), default_stock_data, error()$fatal)
      }
    })
    
    final_df
  })
}

format_file_input <- function(tag, ns) {
  tag <- tagAppendChildren(
    tag, .cssSelector = ".input-group",
    actionButton(ns("reset"), label = "", icon = icon("xmark"))
  ) %>%
    tagAppendAttributes(.cssSelector = ".input-group", class = "box_row")
  tag$children[[2]]$children[[1]] <- 
    tagAppendAttributes(tag$children[[2]]$children[[1]], 
                        class = "formatted_file_input")
  tag
}
