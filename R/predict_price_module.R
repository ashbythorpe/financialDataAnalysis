#' Generate stock price predictions
#' 
#' Allows the user to specify a start date and an end date, then predicts the
#' price of a stock over the specified period, and plots the results.
#' 
#' @param id The namespace of the module.
#' @param stock The selected stock to generate predictions for.
#' @param interactive Whether the created plot should be interactive.
#' 
#' @name predict_price_module
#' @export
predict_price_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("frequency"), "Frequency", choices = c("daily", "monthly")) %>%
      add_info_to_label("frequency"),
    tabsetPanel(
      id = ns("wizard"), type = "hidden",
      tabPanelBody(
        "daily",
        shinyWidgets::airDatepickerInput(
          ns("dates_daily"), label = "Prediction range", range = TRUE, 
          firstDay = 1, value = c(lubridate::today(), lubridate::today() + 30),
          minDate = min(daily_training_data$ref_date, na.rm = TRUE)
        )
      ),
      tabPanelBody(
        "monthly",
        shinyWidgets::airMonthpickerInput(
          ns("dates_monthly"), range = TRUE, label = "Prediction range",
          value = c(lubridate::today(), lubridate::add_with_rollback(
            lubridate::today(), months(6)
          )),
          minDate = min(monthly_training_data$ref_date, na.rm = TRUE)
        )
      )
    ),
    actionButton(ns("predict"), "Predict prices"),
    br(),
    tabsetPanel(
      type = "hidden", id = ns("plot_wizard"),
      tabPanelBody(
        "interactive",
        div(
          id = "predict_waiter",
          plotly::plotlyOutput(ns("plot"))
        )
      ),
      tabPanelBody(
        "normal",
        plotOutput(ns("normal_plot"))
      )
    )
  )
}

#' @name predict_price_module
#' @export
predict_price_server <- function(id, stock, interactive) {
  moduleServer(id, function(input, output, session) {
    hostess1 <- waiter::Hostess$new()
    hostess2 <- waiter::Hostess$new()
    
    hostess <- hostess_obj$new(hostess1, hostess2)
    
    waiter <- waiter::Waiter$new(
      "predict_waiter", color = "white",
      html = hostess1$get_loader(text_color = "black")
    )
    
    normal_waiter <- waiter::Waiter$new(
      session$ns("normal_plot"), color = "white",
      html = hostess2$get_loader(text_color = "black")
    )
    
    interactive_value <- reactive({
      x <- tryCatch(interactive(), error = function(c) TRUE)
      if(!is.logical(x)) {
        x <- TRUE
      }
      x
    })
    
    observe({
      if(input$frequency %in% c("daily", "monthly")) {
        updateTabsetPanel(session, "wizard", input$frequency)
      }
    })
    
    observe({
      if(!interactive_value()) {
        updateTabsetPanel(session, "plot_wizard", selected = "normal")
      } else {
        updateTabsetPanel(session, "plot_wizard", selected = "interactive")
      }
    })
    
    plot <- reactive({
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
        
        req(from >= min(daily_training_data$ref_date, na.rm = TRUE))
        
        if(interactive_value()) {
          waiter$show()
          hostess$set_active(1)
        } else {
          normal_waiter$show()
          hostess$set_active(2)
        }
        
        hostess$start()
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
        
        req(from >= lubridate::round_date(
          min(monthly_training_data$ref_date, na.rm = TRUE), "month"
        ))
        
        if(interactive_value()) {
          waiter$show()
          hostess$set_active(1)
        } else {
          normal_waiter$show()
          hostess$set_active(2)
        }
        
        hostess$start()
        hostess$set(0)
        
        preds <- predict_price(
          stock(), start_date = from, end_date = to, freq = "monthly", 
          hostess = hostess
        )
      }
      
      plot_predictions(preds)
    }) %>%
      bindEvent(input$predict)
    
    output$plot <- plotly::renderPlotly({
      req(plot(), !identical(interactive_value(), FALSE))
      p <- plotly::ggplotly(plot(), source = "predict")
      hostess$set(95)
      plotly::event_register(p, "plotly_afterplot")
      p
    }) %>%
      bindEvent(plot())
    
    observe({
      hostess$set(99)
      waiter$hide()
    }) %>% 
      bindEvent({
        req(plot(), !identical(interactive_value(), FALSE))
        plotly::event_data("plotly_afterplot", source = "predict", 
                           priority = "event")
      })
    
    output$normal_plot <- renderPlot({
      req(plot(), identical(interactive_value(), FALSE))
      hostess$set(97)
      plot()
    }) %>%
      bindEvent(plot())
  })
}
