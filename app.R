# app/app.R
# ---------------------------------------------------------------
# Shiny dashboard for the Hotel Demand Instability project.
#
# DESIGN NOTES FOR FIRST-TIME SHINY USER:
#  - This is a single-file app (no modules). Easier to debug.
#  - All model objects are loaded ONCE at startup from .rds.
#    The app NEVER refits a model — it only displays results.
#  - 3 tabs total (Overview / Drivers / Anomalies).
#  - If something breaks during the live demo: each tab is
#    independent, so failure on one doesn't kill the others.
#
# Before running: ensure scripts 01-10 have completed and their
# .rds outputs exist under data/processed/.
# ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(glmnet)
  library(here)
})
narrative <- function(...) {
  div(class = "section-narrative", ...)
}

# ---- Load pre-computed artifacts -------------------------------
# All heavy lifting happens BEFORE the UI is built. App startup
# should be <5 seconds. If it's slow, that's a sign you put
# computation in the wrong place.

booking_level <- readRDS(here("data", "processed", "booking_level.rds"))
weekly_anom   <- readRDS(here("data", "processed", "anomalies.rds"))
enet          <- readRDS(here("data", "processed", "model_enet.rds"))

weekly_preds  <- weekly_anom$weekly_with_anomalies
anom_summary  <- weekly_anom$anomaly_summary

APP_DATA <- tryCatch(
  initial_load(),
  error = function(e) {
    message("[load] FAILED: ", conditionMessage(e))
    list(error = conditionMessage(e))
  }
)

DATA_OK <- is.null(APP_DATA$error)

# ---- UI definition ---------------------------------------------
ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = "Hotel Demand Instability",
    titleWidth = 280
  ),

  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview",         tabName = "overview",  icon = icon("dashboard")),
      #menuItem("Cancellation Drivers", tabName = "drivers", icon = icon("chart-bar")),
      menuItem("Predictions",          tabName = "predictions", icon = icon("magic")),
      menuItem("Volatility & Anomalies", tabName = "anomalies", icon = icon("exclamation-triangle"))
    ),
    hr(),
    selectInput("hotel_filter", "Hotel:",
                choices = c("All", sort(unique(booking_level$hotel))),
                selected = "All"),
    dateRangeInput("date_filter", "Booking date range:",
                   start  = min(booking_level$booking_date, na.rm = TRUE),
                   end    = max(booking_level$booking_date, na.rm = TRUE),
                   format = "yyyy-mm-dd")
  ),

  dashboardBody(
    tabItems(

      # ===== TAB 1: OVERVIEW =====
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("box_total_bookings", width = 4),
          valueBoxOutput("box_cancel_rate",    width = 4),
          valueBoxOutput("box_n_anomalies",    width = 4)
        ),
        fluidRow(
          box(title  = "Weekly bookings and cancellations",
              width  = 12,
              status = "primary",
              solidHeader = TRUE,
              plotlyOutput("ts_plot", height = "500px"))
        )
      ),

      # ===== TAB 2: CANCELLATION DRIVERS =====
      tabItem(
        tabName = "predictions",
        h2("Live Predictions"),
        narrative(
          "Score a hypothetical booking with the trained Elastic Net (Model B) ",
          "and inspect the next-week NB cancellation forecast under user-controlled inputs. ",
          "All predictions use the cached fitted models without re-training."
        ),
        fluidRow(
          box(width = 4, status = "primary", solidHeader = TRUE,
              title = "Booking-level inputs (Elastic Net Model B)",
              selectInput("pred_hotel", "Hotel:",
                          choices = if (DATA_OK) sort(unique(APP_DATA$features$hotel))
                          else c("City Hotel", "Resort Hotel")),
              selectInput("pred_segment", "Market segment:",
                          choices = if (DATA_OK) sort(unique(APP_DATA$features$market_segment))
                          else c("Online TA")),
              selectInput("pred_channel", "Distribution channel:",
                          choices = if (DATA_OK) sort(unique(APP_DATA$features$distribution_channel))
                          else c("TA/TO")),
              sliderInput("pred_lead_time", "Lead time (days):",
                          min = 0, max = 500, value = 90),
              sliderInput("pred_adr",       "Average daily rate (ADR):",
                          min = 0, max = 500, value = 100),
              sliderInput("pred_prior_cancel", "Prior cancellation rate:",
                          min = 0, max = 1, value = 0.0, step = 0.05),
              sliderInput("pred_commitment", "Commitment score (z-composite):",
                          min = -3, max = 6, value = 0, step = 0.1),
              numericInput("pred_complexity", "Booking complexity (guests + nights):",
                           value = 4, min = 1, max = 30, step = 1),
              sliderInput("pred_month", "Booking month:",
                          min = 1, max = 12, value = 6),
              checkboxInput("pred_repeat", "Effective repeat guest", value = FALSE),
              checkboxInput("pred_hol_book",   "Source-country holiday at booking", value = FALSE),
              checkboxInput("pred_hol_arr",    "Source-country holiday at arrival", value = FALSE),
              checkboxInput("pred_evt_book",   "Event active at booking",  value = FALSE),
              checkboxInput("pred_evt_arr",    "Event active at arrival",  value = FALSE),
              sliderInput("pred_weeks_to_evt", "Signed weeks to nearest event:",
                          min = -52, max = 52, value = 0, step = 1)
          ),
          box(width = 8, status = "primary", solidHeader = TRUE,
              title = "Predicted cancellation probability",
              fluidRow(
                valueBoxOutput("pred_prob_box", width = 6),
                valueBoxOutput("pred_class_box", width = 6)
              ),
              plotlyOutput("pred_gauge", height = 250),
              tags$hr(),
              h4("Top driver decomposition (Model B coefficients x feature value)"),
              plotlyOutput("pred_driver_plot", height = 360)
          )
        ),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Weekly aggregate prediction (NB)",
              tags$p("Configure a hypothetical week's market mix and inspect the NB-predicted rate:"),
              fluidRow(
                column(3, sliderInput("nbp_lead", "Avg lead time:",
                                      min = 0, max = 365, value = 90)),
                column(3, sliderInput("nbp_ota", "Share Online TA:",
                                      min = 0, max = 1, value = 0.5, step = 0.05)),
                column(3, sliderInput("nbp_corp", "Share Corporate:",
                                      min = 0, max = 1, value = 0.05, step = 0.05)),
                column(3, sliderInput("nbp_groups", "Share Groups:",
                                      min = 0, max = 1, value = 0.1, step = 0.05))
              ),
              fluidRow(
                column(3, sliderInput("nbp_holdens", "Holiday density:",
                                      min = 0, max = 1, value = 0.05, step = 0.05)),
                column(3, checkboxInput("nbp_evt", "Event-active week", FALSE)),
                column(3, selectInput("nbp_hotel", "Hotel:",
                                      choices = c("City Hotel", "Resort Hotel"))),
                column(3, numericInput("nbp_bookings", "Weekly bookings (volume):",
                                       value = 200, min = 5, max = 2000, step = 10))
              ),
              fluidRow(
                valueBoxOutput("nbp_pred_count", width = 6),
                valueBoxOutput("nbp_pred_rate",  width = 6)
              )
          )
        )
      ),

      # ===== TAB 3: VOLATILITY & ANOMALIES =====
      tabItem(
        tabName = "anomalies",
        fluidRow(
          box(title  = "NB residuals over time (anomalies in red)",
              width  = 12,
              status = "primary",
              solidHeader = TRUE,
              plotlyOutput("anom_plot", height = "450px"),
              p(em("Threshold: |deviance residual| > 2. ",
                   "Dotted lines mark curated events."))
          )
        ),
        fluidRow(
          box(title = "Top anomalous weeks",
              width = 12,
              status = "warning",
              solidHeader = TRUE,
              DT::dataTableOutput("anom_table"))
        )
      )
    ) # end tabItems
  )   # end dashboardBody
)

# ---- Server logic ----------------------------------------------
server <- function(input, output, session) {

  # Filtered booking-level data (used by overview tab)
  filtered_bookings <- reactive({
    df <- booking_level
    if (input$hotel_filter != "All") {
      df <- df |> filter(hotel == input$hotel_filter)
    }
    df |>
      filter(booking_date >= input$date_filter[1],
             booking_date <= input$date_filter[2])
  })

  # ---- Tab 1 reactives ----
  output$box_total_bookings <- renderValueBox({
    valueBox(
      value    = format(nrow(filtered_bookings()), big.mark = ","),
      subtitle = "Bookings in selection",
      icon     = icon("bed"),
      color    = "blue"
    )
  })

  output$box_cancel_rate <- renderValueBox({
    df <- filtered_bookings()
    rate <- if (nrow(df) == 0) NA else mean(df$is_canceled)
    valueBox(
      value    = if (is.na(rate)) "—" else paste0(round(100 * rate, 1), "%"),
      subtitle = "Cancellation rate",
      icon     = icon("ban"),
      color    = "red"
    )
  })
  # ---- PREDICTIONS outputs -------------------------------------------------
  build_pred_row <- reactive({
    x_cols <- APP_DATA$enet$mod_B$x_cols
    row <- matrix(0, nrow = 1, ncol = length(x_cols),
                  dimnames = list(NULL, x_cols))

    set_if <- function(colname, val) {
      if (colname %in% x_cols) row[1, colname] <<- val
    }

    set_if("prior_cancel_rate", input$pred_prior_cancel)
    set_if("commitment_score",  input$pred_commitment)
    set_if("booking_complexity", input$pred_complexity)
    set_if("lead_time",         input$pred_lead_time)
    set_if("adr",               input$pred_adr)
    set_if("booking_month_sin", sin(2 * pi * input$pred_month / 12))
    set_if("booking_month_cos", cos(2 * pi * input$pred_month / 12))
    set_if("is_repeated_guest_effective",
           as.integer(isTRUE(input$pred_repeat)))
    set_if("source_country_holiday_at_booking",
           as.integer(isTRUE(input$pred_hol_book)))
    set_if("source_country_holiday_at_arrival",
           as.integer(isTRUE(input$pred_hol_arr)))
    set_if("event_active_at_booking",
           as.integer(isTRUE(input$pred_evt_book)))
    set_if("event_active_at_arrival",
           as.integer(isTRUE(input$pred_evt_arr)))
    set_if("weeks_to_nearest_event_booking", input$pred_weeks_to_evt)

    # Categorical one-hot columns: hotel<Level>, market_segment<Level>, etc.
    set_if(paste0("hotel",                input$pred_hotel),    1)
    set_if(paste0("market_segment",       input$pred_segment),  1)
    set_if(paste0("distribution_channel", input$pred_channel),  1)

    # Lead time bucket
    bucket <- cut(input$pred_lead_time,
                  breaks = c(-Inf, 7, 30, 90, 180, Inf),
                  labels = c("0-7", "8-30", "31-90", "91-180", "180+"),
                  right = TRUE, ordered_result = TRUE)
    bucket_col <- paste0("lead_time_bucket", as.character(bucket))
    # Also try ordered-factor-style names (.L, .Q, .C, ^4)
    poly_cols <- grep("^lead_time_bucket", x_cols, value = TRUE)
    if (length(poly_cols) > 0 && all(grepl("\\.[LQC]|\\^4", poly_cols))) {
      # ordered polynomial contrasts: skip (uncommon for model.matrix~.-1)
    } else {
      set_if(bucket_col, 1)
    }
    row
  })

  pred_prob <- reactive({
    row <- build_pred_row()
    as.numeric(predict(APP_DATA$enet$mod_B$cv, newx = row,
                       s = "lambda.min", type = "response"))
  })

  output$pred_prob_box <- renderValueBox({
    p <- pred_prob()
    valueBox(sprintf("%.1f%%", 100 * p),
             "Predicted cancellation probability",
             icon = icon("percent"),
             color = if (p > 0.5) "red" else "green")
  })

  output$pred_class_box <- renderValueBox({
    p <- pred_prob()
    valueBox(if (p > 0.5) "Likely cancel" else "Likely honoured",
             sprintf("At 0.5 decision threshold (p = %.3f)", p),
             icon = icon("gavel"),
             color = if (p > 0.5) "red" else "green")
  })

  output$pred_gauge <- renderPlotly({
    p <- pred_prob()
    plot_ly(
      type   = "indicator",
      mode   = "gauge+number",
      value  = round(100 * p, 1),
      number = list(suffix = "%"),
      gauge  = list(
        axis  = list(range = c(0, 100)),
        bar   = list(color = if (p > 0.5) PAL$accent else PAL$primary),
        steps = list(
          list(range = c(0, 25),  color = "#E8F1FA"),
          list(range = c(25, 50), color = "#C9DEEF"),
          list(range = c(50, 75), color = "#F8CFCF"),
          list(range = c(75, 100), color = "#EFAAAA")
        ),
        threshold = list(line = list(color = "black", width = 3),
                         thickness = 0.8, value = 50)
      )
    ) |>
      layout(margin = list(l = 30, r = 30, t = 30, b = 10))
  })

  output$pred_driver_plot <- renderPlotly({
    row <- build_pred_row()
    coefs <- APP_DATA$enet$coefs_B
    contribs <- coefs |>
      mutate(value = ifelse(term %in% colnames(row), row[1, term], 0),
             contribution = estimate * value) |>
      filter(contribution != 0) |>
      arrange(desc(abs(contribution))) |>
      head(12) |>
      mutate(term = factor(term, levels = rev(term)),
             sign = ifelse(contribution > 0, "Increases risk", "Decreases risk"))
    validate(need(nrow(contribs) > 0,
                  "No non-zero feature contributions for current inputs."))
    p <- ggplot(contribs, aes(x = contribution, y = term, fill = sign,
                              text = sprintf("%s<br>value = %.3f<br>coef = %.4f<br>contribution = %.4f",
                                             term, value, estimate, contribution))) +
      geom_col() +
      geom_vline(xintercept = 0, colour = "grey30") +
      scale_fill_manual(values = c("Increases risk" = PAL$accent,
                                   "Decreases risk" = PAL$primary), name = NULL) +
      labs(title = NULL,
           x = "Contribution to log-odds (coef x feature value)", y = NULL) +
      theme_project()
    ggplotly(p, tooltip = "text")
  })

  nbp_prediction <- reactive({
    nd <- tibble(
      weekly_bookings        = input$nbp_bookings,
      weekly_avg_lead_time   = input$nbp_lead,
      weekly_share_OTA       = input$nbp_ota,
      weekly_share_corporate = input$nbp_corp,
      weekly_share_groups    = input$nbp_groups,
      weekly_holiday_density = input$nbp_holdens,
      event_active_week      = as.integer(isTRUE(input$nbp_evt)),
      hotel                  = factor(input$nbp_hotel,
                                      levels = c("City Hotel", "Resort Hotel"))
    )
    mu <- predict(APP_DATA$nb$model, newdata = nd, type = "response")
    rate <- mu / input$nbp_bookings
    list(count = mu, rate = rate)
  })

  output$nbp_pred_count <- renderValueBox({
    valueBox(sprintf("%.1f", nbp_prediction()$count),
             "Predicted cancellations for the week",
             icon = icon("calendar-times"), color = "red")
  })
  output$nbp_pred_rate <- renderValueBox({
    valueBox(sprintf("%.1f%%", 100 * nbp_prediction()$rate),
             "Implied cancellation rate",
             icon = icon("percent"), color = "navy")
  })

  output$box_n_anomalies <- renderValueBox({
    df <- weekly_preds
    if (input$hotel_filter != "All") {
      df <- df |> filter(hotel == input$hotel_filter)
    }
    df <- df |>
      filter(booking_week >= input$date_filter[1],
             booking_week <= input$date_filter[2])
    valueBox(
      value    = sum(df$is_anomaly),
      subtitle = "Anomalous weeks",
      icon     = icon("exclamation-triangle"),
      color    = "yellow"
    )
  })

  output$ts_plot <- renderPlotly({
    df <- weekly_preds
    if (input$hotel_filter != "All") {
      df <- df |> filter(hotel == input$hotel_filter)
    }
    df <- df |>
      filter(booking_week >= input$date_filter[1],
             booking_week <= input$date_filter[2])

    p <- ggplot(df, aes(x = booking_week)) +
      geom_line(aes(y = weekly_bookings,      color = "Bookings")) +
      geom_line(aes(y = weekly_cancellations, color = "Cancellations")) +
      scale_color_manual(values = c("Bookings" = "grey40",
                                    "Cancellations" = "#C00000"),
                         name = NULL) +
      facet_wrap(~ hotel, ncol = 1, scales = "free_y") +
      labs(x = NULL, y = "Count / week") +
      theme_minimal()
    ggplotly(p) |> layout(legend = list(orientation = "h", x = 0.3, y = 1.1))
  })

  output$period_narrative <- renderText({
    df <- filtered_bookings()
    if (nrow(df) == 0) return("No bookings in the selected window.")
    rate <- mean(df$is_canceled)
    n    <- nrow(df)
    span <- paste(format(input$date_filter[1]), "to",
                  format(input$date_filter[2]))
    hot  <- if (input$hotel_filter == "All") "both hotels"
            else input$hotel_filter
    paste0("Between ", span, ", ", hot, " recorded ",
           format(n, big.mark = ","), " bookings with an overall ",
           "cancellation rate of ", round(100 * rate, 1), "%.")
  })

  # ---- Tab 2 reactives ----
  output$coef_plot <- renderPlot({
    df <- enet$coefs
    df <- df[order(-abs(df$estimate)), ]
    df <- head(df, 15)
    df$term <- factor(df$term, levels = rev(df$term))
    ggplot(df, aes(x = estimate, y = term, fill = estimate > 0)) +
      geom_col() +
      geom_vline(xintercept = 0, color = "grey30") +
      scale_fill_manual(values = c("TRUE" = "#C00000", "FALSE" = "#1F4E79"),
                        labels = c("TRUE" = "↑ risk", "FALSE" = "↓ risk"),
                        name = NULL) +
      labs(x = "Standardized coefficient", y = NULL) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })

  # What-if prediction.  Build a single design-matrix row matching the
  # Elastic Net's column structure, then predict.  We do this cleanly
  # by reusing one training row as a template and overwriting fields.
  output$wi_pred <- renderText({
    # Build a minimal mock data frame with the input values, then
    # convert to a model matrix the same way 06_model_enet.R did.
    mock <- booking_level |>
      slice(1) |>
      mutate(
        lead_time              = input$wi_lead_time,
        total_of_special_requests = input$wi_special_req,
        booking_complexity     = input$wi_complexity,
        market_segment         = input$wi_segment,
        hotel                  = input$wi_hotel
      )

    # Match the Elastic Net coefficient names — we look up by intersection
    # since we only need an approximate prediction for demo purposes.
    coefs <- enet$coefs
    # Crude scoring: dot-product of available named matches
    score <- with(mock, {
      0.0 +
        coefs$estimate[coefs$term == "lead_time"]              * input$wi_lead_time +
        coefs$estimate[coefs$term == "booking_complexity"]     * input$wi_complexity
    })
    # For a robust demo, scale into a probability with a logistic squash
    # of the linear predictor centered at the dataset's logit base rate
    base <- log(mean(booking_level$is_canceled) /
                  (1 - mean(booking_level$is_canceled)))
    # Rescale the score by predictor std devs to keep magnitudes sane
    lp <- base + 0.01 * sum(score, na.rm = TRUE)
    p_hat <- 1 / (1 + exp(-lp))
    paste0(round(100 * p_hat, 1), "%")
  })

  output$perf_table <- renderTable({
    data.frame(
      Metric = c("ROC-AUC", "PR-AUC", "Brier score"),
      Value  = c(round(enet$auc,   3),
                 round(enet$pr,    3),
                 round(enet$brier, 4))
    )
  })

  # ---- Tab 3 reactives ----
  output$anom_plot <- renderPlotly({
    df <- weekly_preds
    if (input$hotel_filter != "All") {
      df <- df |> filter(hotel == input$hotel_filter)
    }
    p <- ggplot(df, aes(x = booking_week, y = deviance_resid_all,
                        color = is_anomaly)) +
      geom_hline(yintercept = c(-2, 2),
                 linetype = "dashed", color = "grey50") +
      geom_point(alpha = 0.7) +
      scale_color_manual(values = c("TRUE" = "#C00000",
                                    "FALSE" = "grey60"),
                         name = NULL) +
      facet_wrap(~ hotel, ncol = 1) +
      labs(x = NULL, y = "Deviance residual") +
      theme_minimal()
    ggplotly(p)
  })

  output$anom_table <- DT::renderDataTable({
    df <- anom_summary
    if (input$hotel_filter != "All") {
      df <- df |> filter(hotel == input$hotel_filter)
    }
    DT::datatable(df,
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })
}

# ---- Run -------------------------------------------------------
shinyApp(ui, server)
