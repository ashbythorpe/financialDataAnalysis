data_input_ui <- function(id){
  ns <- NS(id)
  accepted_files <- c(
    ".csv", ".xls", ".xlsx", ".xlsm", ".xltx", ".xltm"
  )
  
  box(title = "Upload data",
    fileInput(ns("files"), "Upload files:", multiple = T, accept = accepted_files),
    textOutput("fatal") %>%
      tagAppendAttributes(class = "error"),
    textOutput("nonfatal") %>%
      tagAppendAttributes(class = "warning"),
    p("Combine files with default data?"),
    shinyWidgets::prettyToggle(ns("combine"), "Yes", "No")
  )
}

data_input_server <- function(id){
  moduleServer(id, function(input, output, session){
    dfs <- reactive({
      req(input$files, input$combine)
      read_files(input$files$datapath)
    })
    
    transformed_df <- reactive({
      dfs() %>%
        combine_if_multiple() %>%
        combine_if_specified(default_stock_data, input$combine) %>%
        transform_df(default_stock_data)
    })
    
    error <- reactive({
      get_error(input$files$datapath, dfs(), transformed_df())
    })
    
    output$fatal <- reactive({
      error()$fatal
    })
    
    output$nonfatal <- reactive({
      error()$nonfatal
    })
    
    final_df <- reactive({
      validate_df(transformed_df(), default_stock_data, error()$fatal)
    })
    
    final_df
  })
}
