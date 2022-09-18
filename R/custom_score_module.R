custom_score_ui <- function(id){
  ns <- NS(id)
  div(id = "row_container",
      custom_row_ui("row_1"))
}

custom_score_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    values <- reactiveValues()
    values$rows <- 1
    
    observe({
      n <- max(values$rows) + 1
      old_selector <- glue::glue("row_{input$add}") %>%
        ns() %>%
        paste0("#", .)
      insertUI(selector, where = "afterEnd",
               custom_row_ui(ns(glue::glue("row_{n}"))))
      values$rows <- append(values$rows, n, after = input$add)
    }) %>%
      bindEvent(input$add)
    
    observe({
      selector <- glue::glue("#row_{input$delete}") %>%
        ns() %>%
        paste0("#", .)
      removeUI(selector)
    }) %>%
      bindEvent(input$delete)
    
    custom_args <- reactive({
      purrr::map(glue::glue("row_{1:values$rows}"), custom_row_server) %>%
        purrr::map(rlang::exec) %>%
        dplyr::bind_rows()
    })
    
    custom_row <- reactive({
      validate_custom_score(custom_args())
    })
    
    custom_row
  })
}

custom_row_ui <- function(id) {
  ns <- NS(id)
  n <- stringr::str_extract(id, "[:digit:]+$") %>%
    as.numeric()
  parent_ns <- parent_ns(id)
  div(id = id,
      fluidRow(
        numericInput(ns("x"), "", value = 0),
        numericInput(ns("y"), "", min = 0, max = 1, value = 0),
        actionButton(
          ns("unused"), "Add row",
          onclick = 
            glue::glue("Shiny.setInputValue({parent_ns('add')}, {n}, 
                   {{priority:'event'}})")),
        actionButton(
          ns("unused2"), "Delete row",
          onclick = 
            glue::glue("Shiny.setInputValue({parent_ns('delete')}, {n}, 
                   {{priority:'event'}})"))
      )
  )
}

custom_row_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    row <- reactive({
      req(input$x, input$y)
      tibble::tibble_row(x = input$x, y = input$y)
    })
    
    row
  })
}
