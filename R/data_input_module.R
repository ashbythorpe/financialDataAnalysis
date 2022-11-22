#' Upload data
#' 
#' A shiny module that contains the Data Input page, allowing the user to 
#' upload files. If no valid files are uploaded, the default stock data is
#' returned.
#' 
#' @seealso [input_data()]
#' 
#' @param id The namespace of the module.
#' 
#' @returns
#' The server returns the data frame that has been uploaded (or the default
#' stock data).
#' 
#' @name data_input_module
#' @export
data_input_ui <- function(id){
  ns <- NS(id)
  accepted_files <- c(
    ".csv", ".xls", ".xlsx", ".xlsm", ".xltx", ".xltm"
  )
  
  box(title = "Upload data",
    fileInput(ns("files"), "Upload files:", multiple = T, 
              accept = accepted_files) %>%
      format_file_input(ns), # Add cancel button
    shinyWidgets::prettySwitch(ns("combine"),
                               label = "Combine files with default data?"),
    textOutput(ns("fatal")) %>%
      tagAppendAttributes(class = "error"),
    textOutput(ns("nonfatal")) %>%
      tagAppendAttributes(class = "warning")
  )
}

#' @name data_input_module
#' @export
data_input_server <- function(id){
  moduleServer(id, function(input, output, session){
    values <- reactiveValues()
    values$files <- NULL
    
    # Set the value of values$files to the inputted files
    observe({
      values$files <- input$files
    })
    
    # Convert the files to data frames
    dfs <- reactive({
      if(is.null(values$files) || is.null(input$combine)) {
        NULL
      } else {
        read_files(values$files$datapath)
      }
    }) %>%
      bindEvent(values$files, ignoreNULL = FALSE)
    
    # Reset the look of the file input and the value of values$files
    observe({
      shinyjs::reset("files")
      values$files <- NULL
    }) %>%
      bindEvent(input$reset)
    
    # Transform and format the data frames
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
    
    # Detect if any errors have occurred in the process
    error <- reactive({
      if(is.null(dfs()) || is.null(transformed_df())) {
        list(fatal = "", nonfatal = "")
      } else {
        get_error(input$files$datapath, dfs(), transformed_df())
      }
    })
    
    # Output errors
    output$fatal <- reactive({
      error()$fatal
    })
    
    output$nonfatal <- reactive({
      error()$nonfatal
    })
    
    # Validate the final data frame
    # If it is invalid, use the default data instead
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

# Create a reset button for the fileInput
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
