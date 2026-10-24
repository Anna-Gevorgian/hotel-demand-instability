suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(glmnet)
  library(MASS)
  library(DT)
  library(here)
})


PAL <- list(
  primary  = "#1F4E79",
  accent   = "#C00000",
  neutral  = "grey50",
  city     = "#1F4E79",
  resort   = "#2E8B57"
)

theme_project <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.line          = element_line(color = "grey30"),
      strip.background   = element_rect(fill = "grey95", color = NA),
      strip.text         = element_text(face = "bold"),
      plot.title         = element_text(face = "bold", size = rel(1.1)),
      plot.subtitle      = element_text(color = "grey40"),
      legend.position    = "bottom"
    )
}

narrative <- function(...) div(class = "narrative-box", ...)
`%||%` <- function(x, y) if (is.null(x)) y else x

v_need <- shiny::need
v_validate <- shiny::validate

print(getwd())

# ── Data loading ──────────────────────────────────────────────────────────────
booking_level <- readRDS("D:/hotel-demand-instability.Rproj/hotel-demand-instability/data/processed/booking_level.rds")
weekly_anom   <- readRDS("D:/hotel-demand-instability.Rproj/hotel-demand-instability/data/processed/anomalies.rds")
enet          <- readRDS("D:/hotel-demand-instability.Rproj/hotel-demand-instability/data/processed/model_enet.rds")
nb_model_obj  <- readRDS("D:/hotel-demand-instability.Rproj/hotel-demand-instability/data/processed/model_nb.rds")

weekly_preds <- weekly_anom$weekly_with_anomalies
anom_summary <- weekly_anom$anomaly_summary

message("=== enet names: ", paste(names(enet), collapse = ", "))
if (!is.null(enet$mod_B)) {
  message("=== enet$mod_B names: ", paste(names(enet$mod_B), collapse = ", "))
} else {
  message("=== enet$mod_B is NULL — checking top-level for cv/x_cols/coefs_B")
  message("    class(enet): ", paste(class(enet), collapse = ", "))
  # Print all top-level names with their class
  for (nm in names(enet)) {
    message(sprintf("    enet$%s  class=%s  len=%s",
                    nm, paste(class(enet[[nm]]), collapse="/"),
                    tryCatch(length(enet[[nm]]), error=function(e) "?")))
  }
}

APP_DATA <- list(
  features = booking_level,
  enet     = enet,
  nb       = nb_model_obj
)

resolve_enet <- function(e) {

  is_cvglmnet <- function(x) inherits(x, "cv.glmnet")

  find_cv <- function(x, path = "enet") {
    if (is_cvglmnet(x)) return(list(cv = x, path = path))
    if (is.list(x)) {
      preferred <- c(
        "mod_B", "model_B", "B", "full", "model_full", "full_model",
        "benchmark", "fit_B", "enet_B", "elastic_net_B", "cv", "fit", "model", "mod"
      )
      nms <- names(x)
      ordered <- unique(c(intersect(preferred, nms), setdiff(nms, preferred)))
      for (nm in ordered) {
        out <- find_cv(x[[nm]], paste0(path, "$", nm))
        if (!is.null(out)) return(out)
      }
    }
    NULL
  }

  find_x_cols <- function(x, cv_obj = NULL) {
    candidate_names <- c(
      "x_cols", "X_cols", "feature_names", "features", "predictors",
      "model_cols", "model_columns", "columns", "terms"
    )

    if (is.list(x)) {
      for (nm in intersect(candidate_names, names(x))) {
        val <- x[[nm]]
        if (is.character(val) && length(val) > 0) return(val)
      }
      # one-level nested search first, so we do not accidentally grab unrelated metadata
      for (child in x) {
        if (is.list(child)) {
          for (nm in intersect(candidate_names, names(child))) {
            val <- child[[nm]]
            if (is.character(val) && length(val) > 0) return(val)
          }
        }
      }
    }

    if (!is.null(cv_obj)) {
      # cv.glmnet keeps feature names in the coefficient matrix / beta matrix.
      cf <- tryCatch(as.matrix(stats::coef(cv_obj, s = "lambda.min")), error = function(err) NULL)
      if (!is.null(cf)) {
        rn <- setdiff(rownames(cf), "(Intercept)")
        if (length(rn) > 0) return(rn)
      }
      rn <- tryCatch(rownames(cv_obj$glmnet.fit$beta), error = function(err) NULL)
      if (!is.null(rn) && length(rn) > 0) return(rn)
    }

    NULL
  }

  if (!is.null(e$mod_B) && !is.null(e$mod_B$cv) && is_cvglmnet(e$mod_B$cv)) {
    message("[enet] Using existing enet$mod_B$cv.")
    if (is.null(e$mod_B$x_cols)) e$mod_B$x_cols <- find_x_cols(e$mod_B, e$mod_B$cv)
  } else {
    # 2) Other known structures / any recursive cv.glmnet object
    found <- find_cv(e)
    if (!is.null(found)) {
      message("[enet] Found cv.glmnet at ", found$path, " — mapping it to enet$mod_B$cv.")
      x_cols <- find_x_cols(e, found$cv)
      e$mod_B <- list(cv = found$cv, x_cols = x_cols)
    } else {
      warning("[enet] No cv.glmnet object found anywhere in model_enet.rds. The cancellation predictor cannot run.")
      return(e)
    }
  }

  if (is.null(e$mod_B$x_cols) || length(e$mod_B$x_cols) == 0) {
    e$mod_B$x_cols <- find_x_cols(e, e$mod_B$cv)
  }

  if (is.null(e$coefs_B)) {
    for (nm in c("coefs_B", "coef_B", "coefficients_B", "coefs", "coef", "coefficients")) {
      if (!is.null(e[[nm]])) {
        e$coefs_B <- e[[nm]]
        message("[enet] Using coefficients from enet$", nm)
        break
      }
    }
  }

  if (is.null(e$coefs_B) && !is.null(e$mod_B$cv)) {
    message("[enet] Rebuilding coefficients from cv.glmnet lambda.min.")
    cf <- as.matrix(stats::coef(e$mod_B$cv, s = "lambda.min"))
    e$coefs_B <- data.frame(
      term = rownames(cf),
      estimate = as.numeric(cf[, 1]),
      stringsAsFactors = FALSE
    ) |>
      dplyr::filter(term != "(Intercept)", estimate != 0)
  }

  message("[enet] Final mod_B$x_cols length: ", length(e$mod_B$x_cols %||% character(0)))
  message("[enet] Final coefs_B available: ", !is.null(e$coefs_B))
  e
}
resolve_nb <- function(nb) {
  is_fit <- function(x) inherits(x, c("negbin", "glm", "lm"))

  if (is_fit(nb)) return(list(model = nb))

  candidate_names <- c(
    "model", "fit", "nb_fit", "nb_model", "model_nb",
    "mod", "final_model", "model_all", "m"
  )

  for (nm in intersect(candidate_names, names(nb))) {
    if (is_fit(nb[[nm]])) {
      message("[nb] Using fitted model at nb$", nm)
      return(modifyList(nb, list(model = nb[[nm]])))
    }
  }

  for (nm in names(nb)) {
    if (is.list(nb[[nm]])) {
      for (sm in intersect(candidate_names, names(nb[[nm]]))) {
        if (is_fit(nb[[nm]][[sm]])) {
          message("[nb] Using fitted model at nb$", nm, "$", sm)
          return(modifyList(nb, list(model = nb[[nm]][[sm]])))
        }
      }
    }
  }

  message("[nb] Could not find a fitted Negative Binomial model. Available names: ",
          paste(names(nb), collapse = ", "))
  nb
}


normalise_coefs <- function(coefs) {
  if (is.null(coefs)) return(NULL)

  if (is.matrix(coefs) || inherits(coefs, "dgCMatrix")) {
    m <- as.matrix(coefs)
    return(data.frame(term = rownames(m), estimate = as.numeric(m[, 1]), stringsAsFactors = FALSE))
  }

  coefs <- as.data.frame(coefs)

  if (!all(c("term", "estimate") %in% names(coefs))) {
    term_col <- intersect(c("term", "feature", "variable", "Variable", "name"), names(coefs))[1]
    est_col  <- intersect(c("estimate", "coef", "coefficient", "Coefficient", "value"), names(coefs))[1]
    if (!is.na(term_col) && !is.na(est_col)) {
      coefs <- coefs |>
        dplyr::rename(term = dplyr::all_of(term_col), estimate = dplyr::all_of(est_col))
    }
  }

  if (!all(c("term", "estimate") %in% names(coefs))) return(NULL)

  coefs |>
    dplyr::mutate(estimate = as.numeric(estimate)) |>
    dplyr::filter(!is.na(term), term != "(Intercept)", !is.na(estimate), estimate != 0)
}

APP_DATA$enet <- resolve_enet(APP_DATA$enet)
APP_DATA$nb   <- resolve_nb(APP_DATA$nb)

# ── CSS ───────────────────────────────────────────────────────────────────────

CUSTOM_CSS <- "
/* ── Google Font ── */
@import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500;600&family=DM+Mono:wght@400;500&display=swap');

body, .content-wrapper, .main-sidebar, label, .form-control,
.selectize-input, p, h1, h2, h3, h4, h5 {
  font-family: 'DM Sans', sans-serif !important;
}

/* ── Layout ── */
.content-wrapper, .right-side { background-color: #F2F5F9; }
.main-header .logo, .main-header .navbar { background-color: #1F4E79 !important; border: none; }
.main-header .logo { font-family: 'DM Sans', sans-serif !important; font-weight: 600; letter-spacing: 0.02em; }
.main-sidebar { background-color: #16385A !important; }
.sidebar-menu > li > a { color: rgba(255,255,255,0.75) !important; font-size: 13px; }
.sidebar-menu > li.active > a,
.sidebar-menu > li > a:hover { color: #FFFFFF !important; background-color: rgba(255,255,255,0.1) !important; }
.sidebar-menu > li > a .fa { color: rgba(255,255,255,0.55); }
.sidebar-menu > li.active > a .fa { color: #FFFFFF; }
.skin-blue .sidebar-menu > li.active > a { border-left: 3px solid #5BA3D9; }

/* sidebar inputs */
.sidebar-form label, .sidebar .form-group label { color: rgba(255,255,255,0.65) !important; font-size: 12px; font-weight: 500; letter-spacing: 0.04em; text-transform: uppercase; }
.main-sidebar .form-control, .main-sidebar .selectize-input { background: rgba(255,255,255,0.08) !important; border: 1px solid rgba(255,255,255,0.15) !important; color: #fff !important; border-radius: 6px; }
.main-sidebar .selectize-dropdown { background: #1F4E79; color: #fff; }
.main-sidebar hr { border-color: rgba(255,255,255,0.15); margin: 10px 16px; }

/* ── Boxes ── */
.box { border-radius: 10px !important; border: none !important; box-shadow: 0 1px 4px rgba(0,0,0,0.08) !important; }
.box-header { border-radius: 10px 10px 0 0 !important; }
.box.box-solid.box-primary > .box-header { background: #1F4E79 !important; }
.box.box-solid.box-primary { border: none !important; }
.box.box-solid.box-warning > .box-header { background: #7B4D00 !important; }
.box-title { font-weight: 600; font-size: 13.5px; letter-spacing: 0.01em; }

/* ── Value boxes ── */
.small-box { border-radius: 10px !important; box-shadow: 0 1px 4px rgba(0,0,0,0.10) !important; }
.small-box h3 { font-family: 'DM Mono', monospace !important; font-weight: 500; }
.small-box p  { font-size: 13px; opacity: 0.9; }
.small-box.bg-blue   { background: #1F4E79 !important; }
.small-box.bg-red    { background: #C00000 !important; }
.small-box.bg-yellow { background: #8B6914 !important; }
.small-box.bg-green  { background: #2E8B57 !important; }
.small-box.bg-navy   { background: #1F4E79 !important; }

/* ── Narrative box ── */
.narrative-box {
  background: #FFFFFF;
  border-left: 4px solid #1F4E79;
  padding: 12px 18px;
  margin: 0 0 18px 0;
  border-radius: 0 8px 8px 0;
  color: #2A2A2A;
  font-size: 13.5px;
  line-height: 1.65;
  box-shadow: 0 1px 3px rgba(0,0,0,0.06);
}

/* ── Tab headings ── */
.content-wrapper h2 { color: #1F4E79; font-weight: 600; font-size: 20px; margin-bottom: 16px; }

/* ── Slider & checkbox ── */
.irs--shiny .irs-bar, .irs--shiny .irs-bar-edge { background: #1F4E79; border-top: 1px solid #1F4E79; border-bottom: 1px solid #1F4E79; }
.irs--shiny .irs-handle { border: 2px solid #1F4E79; }
.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background: #1F4E79; }
input[type='checkbox']:checked { accent-color: #1F4E79; }

/* ── DT table ── */
.dataTables_wrapper { font-size: 12.5px; }
table.dataTable thead th { background: #F2F5F9; color: #1F4E79; font-weight: 600; border-bottom: 2px solid #d0dce9; }
"

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = tags$span(
      tags$img( style = "margin-right:8px; vertical-align:top; opacity:0.9"),
      "Hotel Demand"
    ),
    titleWidth = 260
  ),

  dashboardSidebar(
    width = 260,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview",              tabName = "overview",    icon = icon("chart-area")),
      menuItem("Predictions",      tabName = "predictions", icon = icon("magic")),
      menuItem("Volatility & Anomalies",tabName = "anomalies",  icon = icon("exclamation-triangle"))
    ),
    tags$hr(),
    selectInput("hotel_filter", "Hotel",
                choices  = c("All", sort(unique(booking_level$hotel))),
                selected = "All"),
    dateRangeInput("date_filter", "Booking date range",
                   start  = min(booking_level$booking_date, na.rm = TRUE),
                   end    = max(booking_level$booking_date, na.rm = TRUE),
                   format = "yyyy-mm-dd")
  ),

  dashboardBody(
    tags$head(tags$style(HTML(CUSTOM_CSS))),

    tabItems(

      # ── TAB 1: OVERVIEW ──────────────────────────────────────────────────────
      tabItem(
        tabName = "overview",
        h2("Bookings overview"),
        fluidRow(
          valueBoxOutput("box_total_bookings", width = 4),
          valueBoxOutput("box_cancel_rate",    width = 4),
          valueBoxOutput("box_n_anomalies",    width = 4)
        ),
        fluidRow(
          box(title = "Weekly bookings and cancellations",
              width = 12, status = "primary", solidHeader = TRUE,
              plotlyOutput("ts_plot", height = "460px"))
        )
      ),

      # ── TAB 2: PREDICTIONS ───────────────────────────────────────────────────
      tabItem(
        tabName = "predictions",
        h2("Live predictions"),
        
        narrative(
          tags$strong("Elastic Net Model B"), " scores a single hypothetical booking.",
          " The ", tags$strong("NB panel"), " below forecasts a full week's cancellation",
          " count given a custom market mix. Both models use cached fits — no re-training."
        ),
        
        fluidRow(
          
          # LEFT — INPUTS
          box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            title = tags$span(icon("sliders-h"), " Booking inputs"),
            
            selectInput("pred_hotel", "Hotel",
                        choices = sort(unique(booking_level$hotel))),
            
            selectInput("pred_segment", "Market segment",
                        choices = sort(unique(booking_level$market_segment))),
            
            selectInput("pred_channel", "Distribution channel",
                        choices = sort(unique(booking_level$distribution_channel))),
            
            tags$hr(style = "border-color: rgba(0,0,0,0.08); margin: 8px 0;"),
            
            sliderInput("pred_lead_time", "Lead time (days)",
                        min = 0, max = 500, value = 90),
            
            sliderInput("pred_adr", "Average daily rate",
                        min = 0, max = 500, value = 100),
            
            sliderInput("pred_prior_cancel", "Prior cancel rate",
                        min = 0, max = 1, value = 0, step = 0.05),
            
            sliderInput("pred_commitment", "Commitment score",
                        min = -3, max = 6, value = 0, step = 0.1),
            
            numericInput("pred_complexity",
                         "Booking complexity",
                         value = 4, min = 1, max = 30, step = 1),
            
            sliderInput("pred_month", "Booking month",
                        min = 1, max = 12, value = 6),
            
            tags$hr(style = "border-color: rgba(0,0,0,0.08); margin: 8px 0;"),
            
            checkboxInput("pred_repeat",   "Repeat guest", FALSE),
            checkboxInput("pred_hol_book", "Holiday at booking", FALSE),
            checkboxInput("pred_hol_arr",  "Holiday at arrival", FALSE),
            checkboxInput("pred_evt_book", "Event at booking", FALSE),
            checkboxInput("pred_evt_arr",  "Event at arrival", FALSE),
            
            sliderInput("pred_weeks_to_evt",
                        "Weeks to nearest event",
                        min = -52, max = 52,
                        value = 0, step = 1)
          ),
          
          # RIGHT COLUMN
          column(
            width = 8,
            
            box(
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              title = tags$span(icon("chart-pie"), " Cancellation probability"),
              
              fluidRow(
                valueBoxOutput("pred_prob_box", width = 6),
                valueBoxOutput("pred_class_box", width = 6)
              ),
              
              plotlyOutput("pred_gauge", height = 260)
            ),
            
            box(
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              title = tags$span(icon("calendar-week"),
                                " Weekly aggregate prediction"),
              
              tags$p(style = "color:#555; font-size:13px;",
                     "Configure a hypothetical week's market mix."),
              
              fluidRow(
                column(6,
                       sliderInput("nbp_lead", "Avg lead time",
                                   min = 0, max = 365, value = 90)
                ),
                column(6,
                       numericInput("nbp_bookings",
                                    "Weekly booking volume",
                                    value = 200,
                                    min = 5,
                                    max = 2000,
                                    step = 10)
                )
              ),
              
              fluidRow(
                column(6,
                       sliderInput("nbp_ota", "Share Online TA",
                                   min = 0, max = 1,
                                   value = 0.5, step = 0.05)
                ),
                column(6,
                       sliderInput("nbp_groups", "Share Groups",
                                   min = 0, max = 1,
                                   value = 0.1, step = 0.05)
                )
              ),
              
              fluidRow(
                column(6,
                       sliderInput("nbp_corp", "Share Corporate",
                                   min = 0, max = 1,
                                   value = 0.05, step = 0.05)
                ),
                column(6,
                       sliderInput("nbp_holdens", "Holiday density",
                                   min = 0, max = 1,
                                   value = 0.05, step = 0.05)
                )
              ),
              
              fluidRow(
                column(6,
                       checkboxInput("nbp_evt", "Event-active week", FALSE)
                ),
                column(6,
                       selectInput("nbp_hotel", "Hotel",
                                   choices = c("City Hotel", "Resort Hotel"))
                )
              ),
              
              fluidRow(
                valueBoxOutput("nbp_pred_count", width = 6),
                valueBoxOutput("nbp_pred_rate", width = 6)
              )
            )
          )
        )
      ),

      # ── TAB 3: VOLATILITY & ANOMALIES ────────────────────────────────────────
      tabItem(
        tabName = "anomalies",
        h2("Volatility & anomaly detection"),
        narrative(
          "Deviance residuals from the Negative Binomial model.",
          " Weeks with |residual| > 2 are flagged as anomalous (shown in red).",
          " Dashed lines mark the ±2 threshold."
        ),
        fluidRow(
          box(title = "NB deviance residuals over time",
              width = 12, status = "primary", solidHeader = TRUE,
              plotlyOutput("anom_plot", height = "440px"))
        ),
        fluidRow(
          box(title = "Top anomalous weeks (ranked by |residual|)",
              width = 12, status = "warning", solidHeader = TRUE,
              DT::dataTableOutput("anom_table"))
        )
      )

    ) # end tabItems
  )   # end dashboardBody
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Shared reactive: filtered booking-level data ──
  filtered_bookings <- reactive({
    df <- booking_level
    if (input$hotel_filter != "All") df <- df |> filter(hotel == input$hotel_filter)
    df |> filter(booking_date >= input$date_filter[1],
                 booking_date <= input$date_filter[2])
  })

  # ── TAB 1 ─────────────────────────────────────────────────────────────────

  output$box_total_bookings <- renderValueBox({
    valueBox(format(nrow(filtered_bookings()), big.mark = ","),
             "Bookings in selection", icon = icon("bed"), color = "blue")
  })

  output$box_cancel_rate <- renderValueBox({
    df   <- filtered_bookings()
    rate <- if (nrow(df) == 0) NA else mean(df$is_canceled)
    valueBox(
      if (is.na(rate)) "—" else paste0(round(100 * rate, 1), "%"),
      "Cancellation rate", icon = icon("ban"), color = "red"
    )
  })

  output$box_n_anomalies <- renderValueBox({
    df <- weekly_preds
    if (input$hotel_filter != "All") df <- df |> filter(hotel == input$hotel_filter)
    df <- df |> filter(booking_week >= input$date_filter[1],
                       booking_week <= input$date_filter[2])
    valueBox(sum(df$is_anomaly), "Anomalous weeks",
             icon = icon("exclamation-triangle"), color = "yellow")
  })

  output$ts_plot <- renderPlotly({
    df <- weekly_preds
    if (input$hotel_filter != "All") df <- df |> filter(hotel == input$hotel_filter)
    df <- df |> filter(booking_week >= input$date_filter[1],
                       booking_week <= input$date_filter[2])
    p <- ggplot(df, aes(x = booking_week)) +
      geom_line(aes(y = weekly_bookings,      color = "Bookings"),      linewidth = 0.8) +
      geom_line(aes(y = weekly_cancellations, color = "Cancellations"), linewidth = 0.8) +
      scale_color_manual(values = c("Bookings" = "grey45", "Cancellations" = PAL$accent),
                         name = NULL) +
      facet_wrap(~ hotel, ncol = 1, scales = "free_y") +
      labs(x = NULL, y = "Count / week") +
      theme_project()
    ggplotly(p) |> layout(legend = list(orientation = "h", x = 0.3, y = 1.08))
  })

  # ── TAB 2: Elastic Net predictions ────────────────────────────────────────

  build_pred_row <- reactive({
    v_validate(v_need(!is.null(APP_DATA$enet$mod_B),
                  "Elastic Net model (mod_B) not loaded — check console diagnostics."))
    x_cols <- APP_DATA$enet$mod_B$x_cols
    v_validate(v_need(!is.null(x_cols) && length(x_cols) > 0,
                  "mod_B$x_cols is empty — model object may be incomplete."))
    row    <- matrix(0, nrow = 1, ncol = length(x_cols),
                     dimnames = list(NULL, x_cols))

    set_if <- function(nm, val) if (nm %in% x_cols) row[1, nm] <<- val

    set_if("prior_cancel_rate",                    input$pred_prior_cancel)
    set_if("commitment_score",                     input$pred_commitment)
    set_if("booking_complexity",                   input$pred_complexity)
    set_if("lead_time",                            input$pred_lead_time)
    set_if("adr",                                  input$pred_adr)
    set_if("booking_month_sin",                    sin(2 * pi * input$pred_month / 12))
    set_if("booking_month_cos",                    cos(2 * pi * input$pred_month / 12))
    set_if("is_repeated_guest_effective",          as.integer(isTRUE(input$pred_repeat)))
    set_if("source_country_holiday_at_booking",    as.integer(isTRUE(input$pred_hol_book)))
    set_if("source_country_holiday_at_arrival",    as.integer(isTRUE(input$pred_hol_arr)))
    set_if("event_active_at_booking",              as.integer(isTRUE(input$pred_evt_book)))
    set_if("event_active_at_arrival",              as.integer(isTRUE(input$pred_evt_arr)))
    set_if("weeks_to_nearest_event_booking",       input$pred_weeks_to_evt)

    set_if(paste0("hotel",                input$pred_hotel),   1)
    set_if(paste0("market_segment",       input$pred_segment), 1)
    set_if(paste0("distribution_channel", input$pred_channel), 1)

    # Lead-time bucket (dummy encoding)
    bucket <- cut(input$pred_lead_time,
                  breaks = c(-Inf, 7, 30, 90, 180, Inf),
                  labels = c("0-7","8-30","31-90","91-180","180+"),
                  right = TRUE, ordered_result = TRUE)
    poly_cols <- grep("^lead_time_bucket", x_cols, value = TRUE)
    if (!(length(poly_cols) > 0 && all(grepl("\\.[LQC]|\\^4", poly_cols))))
      set_if(paste0("lead_time_bucket", as.character(bucket)), 1)

    row
  })

  pred_prob <- reactive({
    v_validate(
      v_need(!is.null(APP_DATA$enet$mod_B),
           "enet$mod_B not found — check console for enet structure diagnostics."),
      v_need(!is.null(APP_DATA$enet$mod_B) && !is.null(APP_DATA$enet$mod_B$cv),
           "Elastic Net model not loaded correctly")
    )
    as.numeric(predict(APP_DATA$enet$mod_B$cv, newx = build_pred_row(),
                       s = "lambda.min", type = "response"))
  })

  output$pred_prob_box <- renderValueBox({
    p <- pred_prob()
    valueBox(sprintf("%.1f%%", 100 * p), "Predicted cancel probability",
             icon = icon("percent"), color = if (p > 0.5) "red" else "green")
  })

  output$pred_class_box <- renderValueBox({
    p <- pred_prob()
    valueBox(if (p > 0.5) "Likely cancel" else "Likely honoured",
             sprintf("Threshold 0.50  |  p = %.3f", p),
             icon = icon("gavel"), color = if (p > 0.5) "red" else "green")
  })

  output$pred_gauge <- renderPlotly({
    p <- pred_prob()
    plot_ly(
      type   = "indicator", mode = "gauge+number",
      value  = round(100 * p, 1),
      number = list(suffix = "%", font = list(size = 42, color = "#1F4E79")),
      gauge  = list(
        axis      = list(range = c(0, 100), tickwidth = 1, tickcolor = "#ccc"),
        bar       = list(color = if (p > 0.5) PAL$accent else PAL$primary,
                         thickness = 0.28),
        bgcolor   = "white",
        borderwidth = 0,
        steps = list(
          list(range = c(0,  25),  color = "#EEF4FB"),
          list(range = c(25, 50),  color = "#D9E8F5"),
          list(range = c(50, 75),  color = "#FAE5E5"),
          list(range = c(75, 100), color = "#F5CCCC")
        ),
        threshold = list(line = list(color = "#333", width = 2),
                         thickness = 0.75, value = 50)
      )
    ) |> layout(margin = list(l = 20, r = 20, t = 20, b = 10),
                paper_bgcolor = "rgba(0,0,0,0)")
  })


  # ── TAB 2: NB weekly predictor ────────────────────────────────────────────

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
    v_validate(
      v_need(!is.null(APP_DATA$nb$model) && inherits(APP_DATA$nb$model, c("negbin", "glm", "lm")),
             "NB fitted model was not found. Check model_nb.rds: APP_DATA$nb$model is not a fitted model object.")
    )
    mu <- predict(APP_DATA$nb$model, newdata = nd, type = "response")
    list(count = as.numeric(mu), rate = as.numeric(mu) / input$nbp_bookings)
  })

  output$nbp_pred_count <- renderValueBox({
    valueBox(round(nbp_prediction()$count),
             "Predicted cancellations for the week",
             icon = icon("calendar-times"), color = "red")
  })

  output$nbp_pred_rate <- renderValueBox({
    valueBox(sprintf("%.1f%%", 100 * nbp_prediction()$rate),
             "Implied cancellation rate",
             icon = icon("percent"), color = "navy")
  })

  # ── TAB 3: Anomalies ─────────────────────────────────────────────────────

  output$anom_plot <- renderPlotly({
    df <- weekly_preds
    if (input$hotel_filter != "All") df <- df |> filter(hotel == input$hotel_filter)
    p <- ggplot(df, aes(x = booking_week, y = deviance_resid_all,
                        color = is_anomaly,
                        text  = sprintf("Week: %s\nResidual: %.2f\n%s",
                                        booking_week, deviance_resid_all,
                                        ifelse(is_anomaly, "⚠ Anomaly", "Normal")))) +
      geom_hline(yintercept = c(-2, 2), linetype = "dashed",
                 color = "grey55", linewidth = 0.5) +
      geom_point(alpha = 0.75, size = 2) +
      scale_color_manual(values = c("TRUE" = PAL$accent, "FALSE" = "grey65"),
                         labels  = c("TRUE" = "Anomaly", "FALSE" = "Normal"),
                         name    = NULL) +
      facet_wrap(~ hotel, ncol = 1) +
      labs(x = NULL, y = "Deviance residual") +
      theme_project()
    ggplotly(p, tooltip = "text")
  })

  output$anom_table <- DT::renderDataTable({
    df <- anom_summary
    if (input$hotel_filter != "All") df <- df |> filter(hotel == input$hotel_filter)
    DT::datatable(df, rownames = FALSE,
                  options = list(pageLength = 10, scrollX = TRUE,
                                 dom = "ftip",
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color':'#F2F5F9','color':'#1F4E79'});",
                                   "}")))
  })
}
shinyApp(ui, server)
