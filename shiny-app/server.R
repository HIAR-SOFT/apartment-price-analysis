# ============================================================
# shiny-app/server.R
# Istanbul Real Estate Price Estimator — Server Logic
# ============================================================

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(randomForest)
library(scales)

server <- function(input, output, session) {

  # ── Load data and models ──────────────────────────────────────
  df <- reactive({
    path <- here::here("data", "processed", "all_listings.rds")
    if (file.exists(path)) {
      readRDS(path)
    } else {
      # Return empty demo data if no real data yet
      data.frame(
        listing_type  = character(),
        district      = character(),
        mahalle       = character(),
        price         = numeric(),
        net_area_m2   = numeric(),
        room_count    = numeric(),
        building_age  = numeric(),
        floor_number  = numeric(),
        bathroom_count = numeric(),
        price_per_m2  = numeric(),
        elevator      = logical(),
        parking       = logical(),
        within_site   = logical(),
        heating_type  = character()
      )
    }
  })

  rf_model <- reactive({
    path <- here::here("models", "random_forest_model.rds")
    if (file.exists(path)) readRDS(path) else NULL
  })

  lm_model <- reactive({
    path <- here::here("models", "linear_regression_model.rds")
    if (file.exists(path)) readRDS(path) else NULL
  })

  # ── Populate district dropdown ─────────────────────────────
  observe({
    req(nrow(df()) > 0)
    districts <- df() %>%
      filter(listing_type == input$listing_type) %>%
      count(district) %>%
      filter(n >= 5) %>%
      arrange(district) %>%
      pull(district) %>%
      as.character()

    updateSelectInput(session, "district",
      choices  = setNames(districts, districts),
      selected = districts[1]
    )
  })

  # ── Price prediction ──────────────────────────────────────
  prediction <- eventReactive(input$predict_btn, {
    new_apt <- data.frame(
      net_area_m2    = input$net_area,
      room_count     = as.numeric(input$room_count),
      bathroom_count = input$bathroom_count,
      floor_number   = input$floor_number,
      building_age   = input$building_age,
      elevator       = as.factor(input$elevator),
      parking        = as.factor(input$parking),
      within_site    = as.factor(input$within_site),
      furnished      = as.factor(input$furnished),
      district       = as.factor(input$district),
      heating_type   = as.factor(input$heating_type)
    )

    model <- rf_model()
    if (is.null(model)) {
      return(list(price = NA, low = NA, high = NA, method = "No model"))
    }

    # Align factor levels with training data
    for (col in names(new_apt)) {
      if (is.factor(new_apt[[col]])) {
        train_levels <- levels(model$forest$xlevels[[col]])
        if (!is.null(train_levels)) {
          levels(new_apt[[col]]) <- union(levels(new_apt[[col]]), train_levels)
        }
      }
    }

    log_pred <- predict(model, newdata = new_apt)
    price    <- exp(log_pred)

    # Confidence interval: ±15% based on typical RF prediction spread
    list(
      price  = price,
      low    = price * 0.85,
      high   = price * 1.15,
      method = "Random Forest"
    )
  })

  # ── Similar apartments (same district, ±20% area) ─────────
  similar_apts <- eventReactive(input$predict_btn, {
    req(nrow(df()) > 0)
    df() %>%
      filter(
        listing_type == input$listing_type,
        district     == input$district,
        !is.na(price),
        !is.na(net_area_m2),
        net_area_m2 >= input$net_area * 0.8,
        net_area_m2 <= input$net_area * 1.2
      ) %>%
      arrange(abs(net_area_m2 - input$net_area)) %>%
      head(50)
  })

  # ── Outputs ───────────────────────────────────────────────────

  output$price_display <- renderUI({
    pred <- prediction()
    if (is.na(pred$price)) {
      return(div(class = "price-display", "Run the analysis scripts first"))
    }

    fmt <- function(x) format(round(x), big.mark = ",")

    div(
      div(class = "price-display",
          if (input$listing_type == "satilik") {
            paste0(fmt(pred$price), " TL")
          } else {
            paste0(fmt(pred$price), " TL / month")
          }
      ),
      div(class = "price-range",
          sprintf("Estimated range: %s – %s TL", fmt(pred$low), fmt(pred$high))),
      div(style = "text-align:center; color:#999; font-size:0.85em; margin-top:6px;",
          paste("Method:", pred$method))
    )
  })

  output$stat_avg_price <- renderUI({
    s <- similar_apts()
    avg <- if (nrow(s) > 0) mean(s$price, na.rm = TRUE) else NA
    div(
      div(class = "stat-value",
          if (is.na(avg)) "—" else format(round(avg), big.mark = ",")),
      div(class = "stat-label", "Avg price (district)")
    )
  })

  output$stat_median_price <- renderUI({
    s <- similar_apts()
    med <- if (nrow(s) > 0) median(s$price, na.rm = TRUE) else NA
    div(
      div(class = "stat-value",
          if (is.na(med)) "—" else format(round(med), big.mark = ",")),
      div(class = "stat-label", "Median price")
    )
  })

  output$stat_price_m2 <- renderUI({
    s <- similar_apts()
    ppm2 <- if (nrow(s) > 0) mean(s$price_per_m2, na.rm = TRUE) else NA
    div(
      div(class = "stat-value",
          if (is.na(ppm2)) "—" else format(round(ppm2), big.mark = ",")),
      div(class = "stat-label", "Avg TL / m²")
    )
  })

  output$similar_distribution <- renderPlotly({
    s <- similar_apts()
    pred <- prediction()

    p <- if (nrow(s) == 0) {
      plot_ly() %>% layout(title = "No similar listings found in this district")
    } else {
      plot_ly(s, x = ~price, type = "histogram",
              marker = list(color = "#2196F3", line = list(color = "white", width = 0.5)),
              nbinsx = 30,
              name = "Similar listings") %>%
        add_lines(x = c(pred$price, pred$price), y = c(0, 50),
                  line = list(color = "red", dash = "dash", width = 2),
                  name = "Your estimate") %>%
        layout(
          title = list(text = "Price Distribution of Similar Apartments", font = list(size = 14)),
          xaxis = list(title = "Price (TL)", tickformat = ",.0f"),
          yaxis = list(title = "Count"),
          showlegend = TRUE,
          margin = list(t = 40)
        )
    }
    p
  })

  output$similar_table <- renderDT({
    s <- similar_apts()
    if (nrow(s) == 0) return(data.frame(message = "No similar listings found."))

    s %>%
      select(district, mahalle, price, net_area_m2, room_count_label, building_age, price_per_m2) %>%
      mutate(
        price        = format(round(price),        big.mark = ","),
        price_per_m2 = format(round(price_per_m2), big.mark = ",")
      ) %>%
      rename(
        District = district, Neighbourhood = mahalle,
        "Price (TL)" = price, "Area (m²)" = net_area_m2,
        Rooms = room_count_label, "Age (yrs)" = building_age,
        "TL/m²" = price_per_m2
      ) %>%
      datatable(options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  # ── Market Statistics tab ──────────────────────────────────
  output$market_histogram <- renderPlotly({
    req(nrow(df()) > 0)
    d <- df() %>% filter(listing_type == input$stats_type, !is.na(price))
    plot_ly(d, x = ~price, type = "histogram",
            marker = list(color = "#2196F3"),
            nbinsx = 60) %>%
      layout(
        xaxis = list(title = "Price (TL)", tickformat = ",.0f"),
        yaxis = list(title = "Count")
      )
  })

  output$price_by_room <- renderPlotly({
    req(nrow(df()) > 0)
    d <- df() %>%
      filter(listing_type == "satilik", !is.na(room_count_label), !is.na(price)) %>%
      filter(room_count_label %in% c("1+1", "2+1", "3+1", "4+1", "5+1"))

    plot_ly(d, x = ~room_count_label, y = ~price, type = "box",
            marker = list(opacity = 0.5)) %>%
      layout(
        xaxis = list(title = "Room Count"),
        yaxis = list(title = "Price (TL)", tickformat = ",.0f")
      )
  })

  output$price_per_m2_hist <- renderPlotly({
    req(nrow(df()) > 0)
    d <- df() %>% filter(listing_type == "satilik", !is.na(price_per_m2))
    plot_ly(d, x = ~price_per_m2, type = "histogram",
            marker = list(color = "#FF9800"), nbinsx = 50) %>%
      layout(
        xaxis = list(title = "TL per m²", tickformat = ",.0f"),
        yaxis = list(title = "Count")
      )
  })

  output$correlation_heatmap <- renderPlotly({
    req(nrow(df()) > 0)
    num_cols <- c("price", "net_area_m2", "room_count", "floor_number",
                  "building_age", "bathroom_count", "price_per_m2")
    cor_data <- df() %>%
      select(any_of(num_cols)) %>%
      filter(if_all(everything(), ~ !is.na(.))) %>%
      cor()

    plot_ly(
      x = colnames(cor_data), y = rownames(cor_data), z = cor_data,
      type = "heatmap",
      colorscale = list(c(0, "#E91E63"), c(0.5, "white"), c(1, "#2196F3")),
      zmin = -1, zmax = 1,
      text = round(cor_data, 2), texttemplate = "%{text}"
    ) %>%
      layout(margin = list(l = 100, b = 100))
  })

  # ── District Explorer tab ──────────────────────────────────
  district_summary <- reactive({
    req(nrow(df()) > 0)
    df() %>%
      filter(listing_type == input$district_type, !is.na(price), !is.na(district)) %>%
      group_by(district) %>%
      summarise(
        count        = n(),
        avg_price    = mean(price, na.rm = TRUE),
        median_price = median(price, na.rm = TRUE),
        avg_price_m2 = mean(price_per_m2, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(count >= 5) %>%
      arrange(desc(avg_price))
  })

  output$district_chart <- renderPlotly({
    d <- district_summary() %>% head(20)
    plot_ly(d,
      x = ~avg_price,
      y = ~reorder(district, avg_price),
      type = "bar",
      orientation = "h",
      marker = list(color = "#2196F3", opacity = 0.8),
      text = ~format(round(avg_price), big.mark = ","),
      textposition = "outside"
    ) %>%
      layout(
        xaxis = list(title = "Average Price (TL)", tickformat = ",.0f"),
        yaxis = list(title = ""),
        margin = list(l = 130)
      )
  })

  output$district_table <- renderDT({
    district_summary() %>%
      mutate(across(c(avg_price, median_price, avg_price_m2),
                    ~ format(round(.), big.mark = ","))) %>%
      rename(
        District = district, Count = count,
        "Avg Price" = avg_price, "Median Price" = median_price,
        "Avg TL/m²" = avg_price_m2
      ) %>%
      datatable(options = list(pageLength = 15), rownames = FALSE)
  })

  # ── Model Info tab ─────────────────────────────────────────
  output$lm_summary <- renderPrint({
    m <- lm_model()
    if (is.null(m)) {
      cat("Linear regression model not yet trained.\nRun: Rscript r-analysis/06_regression_model.R")
    } else {
      print(summary(m))
    }
  })

  output$rf_importance <- renderPlotly({
    m <- rf_model()
    if (is.null(m)) return(plot_ly() %>% layout(title = "Model not trained yet"))

    imp <- as.data.frame(importance(m)) %>%
      tibble::rownames_to_column("feature") %>%
      arrange(`%IncMSE`)

    plot_ly(imp, x = ~`%IncMSE`, y = ~reorder(feature, `%IncMSE`),
            type = "bar", orientation = "h",
            marker = list(color = "#FF5722")) %>%
      layout(
        xaxis = list(title = "% Increase in MSE"),
        yaxis = list(title = ""),
        margin = list(l = 130)
      )
  })

  output$model_comparison_plot <- renderPlotly({
    lm_path <- here::here("models", "lm_metrics.rds")
    rf_path <- here::here("models", "rf_metrics.rds")

    if (!file.exists(lm_path) || !file.exists(rf_path)) {
      return(plot_ly() %>%
        layout(title = "Run 06_regression_model.R and 07_random_forest.R first"))
    }

    comparison <- bind_rows(readRDS(lm_path), readRDS(rf_path))

    plot_ly(comparison, x = ~model, y = ~R2, type = "bar",
            marker = list(color = c("#2196F3", "#FF5722")),
            name = "R²") %>%
      layout(
        title = "Model R² Comparison",
        xaxis = list(title = ""),
        yaxis = list(title = "R² Score", range = c(0, 1))
      )
  })
}