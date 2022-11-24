#' Generate stock price predictions
#' 
#' Allows the user to specify a start date and an end date, then predicts the
#' price of a stock over the specified period, and plots the results.
#' 
#' @param id The namespace of the module.
#' @param stock The selected stock to generate predictions for.
#' 
#' @name predict_price_module
#' @export
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
          minDate = min(daily_training_data$ref_date, na.rm = TRUE)
        )
      ),
      tabPanelBody(
        "monthly",
        shinyWidgets::airMonthpickerInput(
          ns("dates_monthly"), range = TRUE,
          value = c(lubridate::today(), lubridate::add_with_rollback(
            lubridate::today(), months(6)
          )),
          minDate = min(monthly_training_data$ref_date, na.rm = TRUE)
        )
      )
    ),
    actionButton(ns("predict"), "Predict prices"),
    br(),
    div(
      id = "predict_waiter",
      plotly::plotlyOutput(ns("plot"))
    )
  )
}

#' @name predict_price_module
#' @export
predict_price_server <- function(id, stock) {
  moduleServer(id, function(input, output, session) {
    hostess <- waiter::Hostess$new()
    
    print(hostess$.__enclos_env__$private$.id)
    
    waiter <- waiter::Waiter$new(
      "predict_waiter",
      html = hostess$get_loader(stroke_color = "#ffffff")
    )
    
    observe({
      if(input$frequency %in% c("daily", "monthly")) {
        updateTabsetPanel(session, "wizard", input$frequency)
      }
    })
    
    plot <- reactive({
      print(input$dates_daily)
      req(input$frequency %in% c("daily", "monthly"))
      
      if(input$frequency == "daily") {
        req(length(input$dates_daily) == 2, 
            input$dates_daily[1], input$dates_daily[2])
        req(stock() %in% daily_training_data$ticker)
        
        if(input$dates_daily[1] <= input$dates_daily[2]) {
          from <- input$dates_daily[1]
          to <- input$dates_daily[2]
        } else {
          from <- input$dates_daily[1]
          to <- input$dates_daily[2]
        }
        
        waiter$show()
        hostess$set(0)
        
        preds <- predict_price(
          stock(), start_date = from, end_date = to, freq = "daily",
          hostess = hostess
        )
        
      } else {
        req(length(input$dates_monthly) == 2, 
            input$dates_monthly[1], input$dates_monthly[2])
        req(stock() %in% monthly_training_data$ticker)
        
        if(input$dates_monthly[1] <= input$dates_monthly[2]) {
          from <- input$dates_monthly[1]
          to <- input$dates_monthly[2]
        } else {
          from <- input$dates_monthly[1]
          to <- input$dates_monthly[2]
        }
        
        waiter$show()
        
        preds <- predict_price(
          stock(), start_date = from, end_date = to, freq = "monthly", 
          hostess = hostess
        )
      }
      
      plot_predictions(preds)
    }) %>%
      bindEvent(input$predict)
    
    output$plot <- plotly::renderPlotly({
      req(plot())
      p <- plotly::ggplotly(plot(), source = "predict")
      hostess$set(95)
      plotly::event_register(p, "plotly_afterplot")
      p
    }) %>%
      bindEvent(plot())
    
    observe({
      hostess$set(100)
      waiter$hide()
    }) %>% 
      bindEvent({
        req(plot())
        plotly::event_data("plotly_afterplot", source = "predict", 
                           priority = "event")
      })
  })
}
