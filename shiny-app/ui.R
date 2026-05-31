# ============================================================
# shiny-app/ui.R
# Istanbul Apartment Sale Price Estimator
# Sales data only — rental tab removed.
# ============================================================

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)

ui <- dashboardPage(
  skin = "blue",
  
  # ----------------------------------------------------------
  # HEADER
  # ----------------------------------------------------------
  dashboardHeader(title = "Istanbul Real Estate"),
  
  # ----------------------------------------------------------
  # SIDEBAR
  # ----------------------------------------------------------
  dashboardSidebar(
    sidebarMenu(
      menuItem("Price Estimator",   tabName = "estimator",  icon = icon("calculator")),
      menuItem("Market Statistics", tabName = "statistics", icon = icon("chart-bar")),
      menuItem("District Explorer", tabName = "districts",  icon = icon("map")),
      menuItem("Model Info",        tabName = "models",     icon = icon("brain"))
    )
  ),
  
  # ----------------------------------------------------------
  # BODY
  # ----------------------------------------------------------
  dashboardBody(
    
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f4f6f9; }
      .box             { border-radius: 8px; }

      /* Price result */
      .price-display {
        font-size: 2.4em; font-weight: 700;
        color: #1565C0; text-align: center; padding: 18px 0 6px;
      }
      .price-range {
        font-size: 1.05em; color: #555; text-align: center; padding-bottom: 4px;
      }
      .price-method {
        font-size: .82em; color: #999; text-align: center; padding-bottom: 10px;
      }
      .price-error {
        font-size: 1.1em; color: #c62828; text-align: center; padding: 20px;
      }

      /* Stat cards */
      .stat-value { font-size: 1.7em; font-weight: 700; color: #1565C0; }
      .stat-label { font-size: .88em; color: #777; margin-top: 2px; }

      /* Tighten slider labels */
      .irs--shiny .irs-single { font-size: 11px; }

      /* Section dividers inside input box */
      .input-section-label {
        font-size: .78em; font-weight: 600; color: #888;
        text-transform: uppercase; letter-spacing: .05em;
        margin: 12px 0 4px;
      }
    "))),
    
    tabItems(
      
      # ========================================================
      # TAB 1 — PRICE ESTIMATOR
      # ========================================================
      tabItem(tabName = "estimator",
              fluidRow(
                
                # --- Input panel ------------------------------------
                box(width = 4, title = "Apartment Details",
                    status = "primary", solidHeader = TRUE,
                    
                    # Location
                    div(class = "input-section-label", "Location"),
                    selectInput("district", "District",
                                choices = c("Loading..." = ""), selected = ""),
                    
                    # Size
                    div(class = "input-section-label", "Size"),
                    sliderInput("gross_area", "Gross Area (m\u00b2)",
                                min = 30, max = 600, value = 120, step = 5),
                    sliderInput("net_area",   "Net Area (m\u00b2)",
                                min = 25, max = 550, value = 100, step = 5),
                    
                    # Layout
                    div(class = "input-section-label", "Layout"),
                    selectInput("room_count", "Number of Rooms",
                                choices  = setNames(1:7,
                                                    c("1","2","3","4","5","6","7+")),
                                selected = 3),
                    numericInput("bathroom_count", "Bathrooms",
                                 value = 1, min = 1, max = 6, step = 1),
                    
                    # Building
                    div(class = "input-section-label", "Building"),
                    sliderInput("building_age", "Building Age (years)",
                                min = 0, max = 80, value = 15, step = 1),
                    selectInput("building_condition", "Condition",
                                choices  = c("New", "Second-hand"),
                                selected = "Second-hand"),
                    selectInput("heating_type", "Heating Type",
                                choices  = c("Loading..." = ""),
                                selected = ""),
                    
                    # Features
                    div(class = "input-section-label", "Features"),
                    checkboxInput("within_site", "In Gated Complex", value = FALSE),
                    
                    # Estimate button
                    br(),
                    actionButton("predict_btn", "Estimate Price",
                                 class = "btn-primary btn-lg btn-block",
                                 icon  = icon("calculator"))
                ),
                
                # --- Output panel -----------------------------------
                column(width = 8,
                       
                       # Price result card
                       box(width = NULL, title = "Price Estimate",
                           status = "success", solidHeader = TRUE,
                           
                           conditionalPanel("input.predict_btn == 0",
                                            div(style = "text-align:center;color:#bbb;padding:40px 0;",
                                                icon("building", class = "fa-3x"),
                                                br(), br(),
                                                p("Fill in the details on the left and click",
                                                  strong("Estimate Price"), "to get a prediction."))
                           ),
                           conditionalPanel("input.predict_btn > 0",
                                            uiOutput("price_display"),
                                            hr(style = "margin: 6px 0;"),
                                            plotlyOutput("similar_distribution", height = "240px")
                           )
                       ),
                       
                       # Similar listings summary stats
                       conditionalPanel("input.predict_btn > 0",
                                        box(width = NULL,
                                            title  = "Similar Listings in Selected District",
                                            status = "info", solidHeader = TRUE,
                                            
                                            fluidRow(
                                              column(4, uiOutput("stat_avg_price")),
                                              column(4, uiOutput("stat_median_price")),
                                              column(4, uiOutput("stat_price_m2"))
                                            ),
                                            br(),
                                            DTOutput("similar_table")
                                        )
                       )
                )
              )
      ),
      
      # ========================================================
      # TAB 2 — MARKET STATISTICS
      # ========================================================
      tabItem(tabName = "statistics",
              fluidRow(
                box(width = 12, title = "Sale Price Distribution",
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("market_histogram", height = "350px"))
              ),
              fluidRow(
                box(width = 6, title = "Price by Number of Rooms",
                    status = "info", solidHeader = TRUE,
                    plotlyOutput("price_by_room", height = "320px")),
                box(width = 6, title = "Price per m\u00b2 Distribution",
                    status = "info", solidHeader = TRUE,
                    plotlyOutput("price_per_m2_hist", height = "320px"))
              ),
              fluidRow(
                box(width = 12, title = "Correlation Matrix",
                    status = "warning", solidHeader = TRUE,
                    plotlyOutput("correlation_heatmap", height = "430px"))
              )
      ),
      
      # ========================================================
      # TAB 3 — DISTRICT EXPLORER
      # ========================================================
      tabItem(tabName = "districts",
              fluidRow(
                box(width = 12, title = "Median Sale Price by District",
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("district_chart", height = "560px"))
              ),
              fluidRow(
                box(width = 12, title = "District Summary",
                    status = "info", solidHeader = TRUE,
                    DTOutput("district_table"))
              )
      ),
      
      # ========================================================
      # TAB 4 — MODEL INFO
      # ========================================================
      tabItem(tabName = "models",
              fluidRow(
                box(width = 12, title = "Model Comparison (R\u00b2)",
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("model_comparison_plot", height = "280px"))
              ),
              fluidRow(
                box(width = 6, title = "Linear Regression — Summary",
                    status = "info", solidHeader = TRUE,
                    verbatimTextOutput("lm_summary")),
                box(width = 6, title = "Random Forest — Feature Importance",
                    status = "warning", solidHeader = TRUE,
                    plotlyOutput("rf_importance", height = "340px"))
              )
      )
      
    ) # end tabItems
  )   # end dashboardBody
)