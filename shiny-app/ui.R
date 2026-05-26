
# shiny-app/ui.R


library(shiny)
library(shinydashboard)
library(plotly)
library(DT)

#  District choices (populated from data at runtime in server) 
# This is a placeholder real choices come from the dataset

ui <- dashboardPage(
  skin = "blue",

  # Header
  dashboardHeader(title = "🏙️ Istanbul Real Estate"),

  # sidebar      
  dashboardSidebar(
    sidebarMenu(
      menuItem("Price Estimator",  tabName = "estimator",  icon = icon("calculator")),
      menuItem("Market Statistics", tabName = "statistics", icon = icon("chart-bar")),
      menuItem("District Explorer", tabName = "districts",  icon = icon("map")),
      menuItem(" Model Info",        tabName = "models",     icon = icon("brain"))
    )
  ),

  #  Body 
  dashboardBody(
    # Custom CSS
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f4f6f9; }
      .box { border-radius: 8px; }
      .price-display {
        font-size: 2.5em;
        font-weight: bold;
        color: #1565C0;
        text-align: center;
        padding: 20px;
      }
      .price-range {
        font-size: 1.1em;
        color: #555;
        text-align: center;
      }
      .stat-value { font-size: 1.8em; font-weight: bold; color: #1565C0; }
      .stat-label { font-size: 0.9em; color: #777; }
    "))),

    tabItems(

      #TAB  Price Estimator
      tabItem(tabName = "estimator",
        fluidRow(
          # Input panel
          box(width = 5, title = "Apartment Details", status = "primary", solidHeader = TRUE,

            selectInput("listing_type", "Listing Type",
              choices = c("For Sale" = "satilik", "For Rent" = "kiralik"),
              selected = "satilik"),

            selectInput("district", "District",
              choices = c("Loading…" = ""), selected = ""),

            selectInput("room_count", "Room Count",
              choices = c("1+1" = 1, "2+1" = 2, "3+1" = 3, "4+1" = 4, "5+1" = 5),
              selected = 2),

            sliderInput("net_area", "Net Area (m²)",
              min = 30, max = 500, value = 100, step = 5),

            sliderInput("building_age", "Building Age (years)",
              min = 0, max = 50, value = 10, step = 1),

            sliderInput("floor_number", "Floor Number",
              min = 0, max = 30, value = 3, step = 1),

            numericInput("bathroom_count", "Bathrooms", value = 1, min = 1, max = 5),

            fluidRow(
              column(6,
                checkboxInput("elevator",    "Elevator",    value = TRUE),
                checkboxInput("parking",     "Parking",     value = FALSE)
              ),
              column(6,
                checkboxInput("within_site", "Gated Complex", value = FALSE),
                checkboxInput("furnished",   "Furnished",   value = FALSE)
              )
            ),

            selectInput("heating_type", "Heating Type",
              choices = c(
                "Central"    = "Merkezi",
                "Combi"      = "Kombi",
                "Floor Heat" = "Yerden Isıtma",
                "Other"      = "Diğer"
              )),

            actionButton("predict_btn", "Estimate Price",
              class = "btn-primary btn-lg btn-block",
              icon = icon("calculator"))
          ),

          # Output panel
          column(width = 7,
            box(width = NULL, title = "Price Estimate", status = "success", solidHeader = TRUE,

              conditionalPanel("input.predict_btn == 0",
                div(style = "text-align:center; color:#aaa; padding:40px;",
                    icon("home", "fa-4x"),
                    br(), br(),
                    "Fill in the apartment details and click 'Estimate Price'")
              ),

              conditionalPanel("input.predict_btn > 0",
                uiOutput("price_display"),
                hr(),
                plotlyOutput("similar_distribution", height = "260px")
              )
            ),

            box(width = NULL, title = "Similar Apartments in Neighborhood",
                status = "info", solidHeader = TRUE,
              conditionalPanel("input.predict_btn > 0",
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

      #TAB Market Statistics
      tabItem(tabName = "statistics",
        fluidRow(
          box(width = 12, title = "Price Distribution", status = "primary",
            selectInput("stats_type", NULL,
              choices = c("For Sale" = "satilik", "For Rent" = "kiralik"),
              selected = "satilik", width = "200px"),
            plotlyOutput("market_histogram", height = "350px")
          )
        ),
        fluidRow(
          box(width = 6, title = "Price by Room Count",
            plotlyOutput("price_by_room", height = "300px")),
          box(width = 6, title = "Price per m² Distribution",
            plotlyOutput("price_per_m2_hist", height = "300px"))
        ),
        fluidRow(
          box(width = 12, title = "Correlation Matrix",
            plotlyOutput("correlation_heatmap", height = "450px"))
        )
      ),

      # District Explorer
      tabItem(tabName = "districts",
        fluidRow(
          box(width = 12, title = "Average Price by District",
            selectInput("district_type", NULL,
              choices = c("For Sale" = "satilik", "For Rent" = "kiralik"),
              width = "200px"),
            plotlyOutput("district_chart", height = "500px"))
        ),
        fluidRow(
          box(width = 12, title = "District Summary Table",
            DTOutput("district_table"))
        )
      ),

      #TAB Model Info 
      tabItem(tabName = "models",
        fluidRow(
          box(width = 12, title = "Model Comparison",
            plotlyOutput("model_comparison_plot", height = "300px"))
        ),
        fluidRow(
          box(width = 6, title = "Linear Regression — Coefficients",
            verbatimTextOutput("lm_summary")),
          box(width = 6, title = "Random Forest — Feature Importance",
            plotlyOutput("rf_importance", height = "350px"))
        )
      )
    )
  )
)