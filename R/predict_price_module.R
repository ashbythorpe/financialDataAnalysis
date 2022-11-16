predict_price_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("frequency"), "", choices = c("daily", "monthly")),
    tabsetPanel(
      id = ns("wizard"), type = "hidden",
      tabPanelBody(
        "daily",
        shinyWidgets::airDatepickerInput(
          ns("dates_daily"), range = TRUE, firstDay = 1, 
          value = c(lubridate::today(), lubridate::today() + 30),
          minDate = min(daily_stock_data$ref_date, na.rm = TRUE)
        )
      ),
      tabPanelBody(
        "monthly",
        shinyWidgets::airMonthpickerInput(
          ns("dates_monthly"), range = TRUE,
          value = c(lubridate::today(), lubridate::add_with_rollback(
            lubridate::today(), months(6)
          )),
          minDate = min(monthly_stock_data$ref_date, na.rm = TRUE)
        )
      )
    ),
    plotly::plotlyOutput(ns("plot"))
  )
}

predict_price_server <- function(id, stock) {
  moduleServer(id, function(input, output, session) {
    observe({
      if(input$frequency %in% c("daily", "monthly")) {
        updateTabsetPanel(session, "wizard", input$frequency)
      }
    })
    
    plot <- reactive({
      req(input$frequency %in% c("daily", "monthly"))
      
      if(input$frequency == "daily") {
        req(length(input$dates_daily) == 2, 
            input$dates_daily[1], input$dates_daily[2])
        req(stock() %in% daily_stock_data$ticker)
        
        if(input$dates_daily[1] <= input$dates_daily[2]) {
          from <- input$dates_daily[1]
          to <- input$dates_daily[2]
        } else {
          from <- input$dates_daily[1]
          to <- input$dates_daily[2]
        }
        
        preds <- predict_price(
          stock(), start_date = from, end_date = to, freq = "daily"
        )
      } else {
        req(length(input$dates_monthly) == 2, 
            input$dates_monthly[1], input$dates_monthly[2])
        req(stock() %in% monthly_stock_data$ticker)
        
        if(input$dates_monthly[1] <= input$dates_monthly[2]) {
          from <- input$dates_monthly[1]
          to <- input$dates_monthly[2]
        } else {
          from <- input$dates_monthly[1]
          to <- input$dates_monthly[2]
        }
        
        preds <- predict_price(
          stock(), start_date = from, end_date = to, freq = "monthly"
        )
      }
      
      plot_predictions(preds)
    })
    
    output$plot <- plotly::renderPlotly({
      req(plot())
      plotly::ggplotly(plot())
    }) %>%
      bindEvent(plot())
  })
}
