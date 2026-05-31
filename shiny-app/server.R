# ============================================================
# shiny-app/server.R
# Istanbul Apartment Sale Price Estimator — sales data only
# ============================================================

library(shiny)
library(dplyr)
library(plotly)
library(DT)
library(randomForest)
library(tidyr)
library(here)

server <- function(input, output, session) {
  
  # ============================================================
  # 1. LOAD DATA & MODELS
  # ============================================================
  df <- reactive({
    p <- here("data", "processed", "all_listings.rds")
    validate(need(file.exists(p),
                  "Data not found. Run 01_data_loading.R first."))
    readRDS(p)
  })
  
  rf_model <- reactive({
    p <- here("models", "random_forest_model.rds")
    if (file.exists(p)) readRDS(p) else NULL
  })
  
  # Saved by 07_random_forest.R: list(features, factor_levels)
  rf_meta <- reactive({
    p <- here("models", "rf_features.rds")
    if (file.exists(p)) readRDS(p) else NULL
  })
  
  lm_model <- reactive({
    p <- here("models", "linear_regression_model.rds")
    if (file.exists(p)) readRDS(p) else NULL
  })
  
  # ============================================================
  # 2. POPULATE DROPDOWNS FROM DATA
  # ============================================================
  observe({
    req(nrow(df()) > 0)
    
    # Districts
    districts <- sort(unique(na.omit(as.character(df()$district))))
    updateSelectInput(session, "district",
                      choices  = districts,
                      selected = districts[1])
    
    # Heating types — collapse rare levels to "Other" to match
    # what the training script does (threshold = 5 rows)
    ht_tab    <- table(na.omit(df()$heating_type))
    known_ht  <- sort(names(ht_tab[ht_tab >= 5 &
                                     !names(ht_tab) %in% c("Unknown","NA","")]))
    ht_choices <- c(known_ht, "Other")
    default_ht <- if ("Combi Boiler" %in% ht_choices) "Combi Boiler" else ht_choices[1]
    updateSelectInput(session, "heating_type",
                      choices  = ht_choices,
                      selected = default_ht)
  })
  
  # ============================================================
  # 3. PREDICTION
  # ============================================================
  prediction <- eventReactive(input$predict_btn, {
    
    model <- rf_model()
    meta  <- rf_meta()
    
    if (is.null(model) || is.null(meta)) {
      return(list(price = NA, low = NA, high = NA,
                  method = "Model not found — run 07_random_forest.R first."))
    }
    
    feats         <- meta$features
    factor_levels <- meta$factor_levels   # named list: col -> char vector
    
    # If factor_levels is NULL the model was saved before we added the fix —
    # re-running 07_random_forest.R will populate it.
    if (is.null(factor_levels)) {
      return(list(price = NA, low = NA, high = NA,
                  method = paste("factor_levels missing from rf_features.rds.",
                                 "Please re-run 07_random_forest.R to regenerate the model.")))
    }
    
    # --- Build prediction row ----------------------------------
    gross <- as.numeric(input$gross_area)
    net   <- as.numeric(input$net_area)
    if (is.na(net) || net > gross) net <- gross   # net cannot exceed gross
    
    # Build as a list first, then as.data.frame — avoids stringsAsFactors
    # being misread as a column name
    new_apt <- as.data.frame(list(
      GrossSquareMeters  = gross,
      NetSquareMeters    = net,
      rooms              = as.numeric(input$room_count),
      bathroom_count     = as.numeric(input$bathroom_count),
      building_age       = as.numeric(input$building_age),
      is_in_complex      = isTRUE(input$within_site),
      heating_type       = as.character(input$heating_type),
      building_condition = as.character(input$building_condition),
      district           = as.character(input$district)
    ), stringsAsFactors = FALSE)
    
    # --- Align factor columns to training levels ---------------
    factor_cols <- intersect(names(factor_levels), feats)
    
    for (col in factor_cols) {
      lvls <- factor_levels[[col]]
      val  <- as.character(new_apt[[col]])
      
      if (!val %in% lvls) {
        fallback <- if ("Other" %in% lvls) "Other" else lvls[1]
        showNotification(
          paste0("'", val, "' unseen in training for ", col,
                 " \u2192 using '", fallback, "'"),
          type = "warning", duration = 5
        )
        val <- fallback
      }
      new_apt[[col]] <- factor(val, levels = lvls)
    }
    
    # --- Guard: all required features present ------------------
    missing <- setdiff(feats, names(new_apt))
    if (length(missing) > 0) {
      return(list(price = NA, low = NA, high = NA,
                  method = paste("Missing features:", paste(missing, collapse = ", "))))
    }
    
    # --- Diagnostics: print everything to console -------------
    cat("\n--- PREDICTION DIAGNOSTICS ---\n")
    cat("feats        :", paste(feats, collapse=", "), "\n")
    cat("factor_levels:", paste(names(factor_levels), collapse=", "), "\n")
    cat("new_apt cols :", paste(names(new_apt), collapse=", "), "\n")
    cat("new_apt types:\n"); print(sapply(new_apt, class))
    cat("new_apt values:\n"); print(new_apt)
    
    # --- Predict (log scale) -----------------------------------
    err_msg  <- NULL
    log_pred <- tryCatch({
      as.numeric(predict(model, newdata = new_apt[, feats, drop = FALSE]))[[1]]
    }, error = function(e) {
      err_msg <<- conditionMessage(e)
      cat("PREDICT ERROR:", err_msg, "\n")
      NA_real_
    })
    
    if (is.na(log_pred)) {
      msg <- if (!is.null(err_msg)) paste("Error:", err_msg)
      else "predict() returned NA"
      return(list(price = NA, low = NA, high = NA, method = msg))
    }
    
    cat("log_pred:", log_pred, "  price:", exp(log_pred), "\n")
    price <- exp(log_pred)
    list(price  = price,
         low    = price * 0.85,
         high   = price * 1.15,
         method = "Random Forest")
  })
  
  # ============================================================
  # 4. SIMILAR LISTINGS
  # ============================================================
  similar <- eventReactive(input$predict_btn, {
    req(nrow(df()) > 0)
    gross <- as.numeric(input$gross_area)
    df() %>%
      filter(
        !is.na(price),
        !is.na(GrossSquareMeters),
        district          == input$district,
        GrossSquareMeters >= gross * 0.8,
        GrossSquareMeters <= gross * 1.2
      ) %>%
      arrange(abs(GrossSquareMeters - gross)) %>%
      head(50)
  })
  
  # ============================================================
  # 5. ESTIMATOR TAB OUTPUTS
  # ============================================================
  output$price_display <- renderUI({
    pred <- prediction()
    fmt  <- function(x) format(round(x), big.mark = ",", scientific = FALSE)
    
    if (is.na(pred$price)) {
      return(div(style = "padding:30px;text-align:center;",
                 div(class = "price-error", pred$method)))
    }
    
    div(
      div(class = "price-display",  paste0(fmt(pred$price), " TL")),
      div(class = "price-range",
          paste0("Range: ", fmt(pred$low), " \u2013 ", fmt(pred$high), " TL")),
      div(class = "price-method", paste("Model:", pred$method))
    )
  })
  
  output$stat_avg_price <- renderUI({
    s   <- similar()
    val <- if (nrow(s) > 0) mean(s$price, na.rm = TRUE) else NA
    div(div(class = "stat-value",
            if (is.na(val)) "-" else format(round(val), big.mark = ",")),
        div(class = "stat-label", "Avg. price (district)"))
  })
  
  output$stat_median_price <- renderUI({
    s   <- similar()
    val <- if (nrow(s) > 0) median(s$price, na.rm = TRUE) else NA
    div(div(class = "stat-value",
            if (is.na(val)) "-" else format(round(val), big.mark = ",")),
        div(class = "stat-label", "Median price"))
  })
  
  output$stat_price_m2 <- renderUI({
    s   <- similar()
    val <- if (nrow(s) > 0) mean(s$price_per_m2, na.rm = TRUE) else NA
    div(div(class = "stat-value",
            if (is.na(val)) "-" else format(round(val), big.mark = ",")),
        div(class = "stat-label", "Avg. TL / m2"))
  })
  
  output$similar_distribution <- renderPlotly({
    s    <- similar()
    pred <- prediction()
    
    if (nrow(s) == 0)
      return(plot_ly() %>%
               layout(title = list(text = "No similar listings in this district.",
                                   font = list(size = 13))))
    
    p <- plot_ly(s, x = ~price, type = "histogram",
                 marker = list(color  = "#2196F3",
                               line   = list(color = "white", width = 0.4)),
                 nbinsx = 25, name = "Similar listings")
    
    if (!is.na(pred$price)) {
      p <- p %>% add_trace(
        type = "scatter", mode = "lines",
        x    = c(pred$price, pred$price),
        y    = c(0, nrow(s) * 0.6),          # rough y ceiling
        line = list(color = "#E53935", dash = "dash", width = 2),
        name = "Your estimate",
        inherit = FALSE
      )
    }
    
    p %>% layout(
      title      = list(text = "Price Distribution of Similar Apartments",
                        font = list(size = 13)),
      xaxis      = list(title = "Price (TL)", tickformat = ",.0f"),
      yaxis      = list(title = "Count"),
      showlegend = TRUE,
      margin     = list(t = 40, r = 20)
    )
  })
  
  output$similar_table <- renderDT({
    s <- similar()
    if (nrow(s) == 0)
      return(data.frame(Message = "No similar listings found."))
    
    # Select only columns that actually exist in s, then rename
    # positionally — DT's colnames must be a plain char vector with
    # length == ncol of the data handed to datatable()
    want_cols <- c("district", "neighbourhood", "price",
                   "GrossSquareMeters", "NetSquareMeters",
                   "rooms", "building_age", "price_per_m2",
                   "furnished", "building_condition")
    col_labels <- c("District", "Neighbourhood", "Price (TL)",
                    "Gross m2", "Net m2", "Rooms", "Age",
                    "TL/m2", "Furnished", "Condition")
    
    present     <- want_cols %in% names(s)
    sel_cols    <- want_cols[present]
    sel_labels  <- col_labels[present]
    
    s %>%
      select(all_of(sel_cols)) %>%
      mutate(across(any_of(c("price", "price_per_m2")),
                    ~ format(round(as.numeric(.)), big.mark = ","))) %>%
      datatable(
        colnames = sel_labels,           # positional vector, same length as ncol
        options  = list(pageLength = 8, scrollX = TRUE, dom = "tip"),
        rownames = FALSE,
        class    = "stripe hover compact"
      )
  })
  
  # ============================================================
  # 6. MARKET STATISTICS TAB
  # ============================================================
  output$market_histogram <- renderPlotly({
    req(nrow(df()) > 0)
    d <- df() %>% filter(!is.na(price))
    plot_ly(d, x = ~price, type = "histogram",
            marker = list(color = "#2196F3"), nbinsx = 60) %>%
      layout(title  = "Sale Price Distribution \u2014 Istanbul",
             xaxis  = list(title = "Price (TL)", tickformat = ",.0f"),
             yaxis  = list(title = "Count"),
             margin = list(t = 50))
  })
  
  output$price_by_room <- renderPlotly({
    req(nrow(df()) > 0)
    d <- df() %>%
      filter(!is.na(rooms), !is.na(price), rooms %in% 1:6) %>%
      mutate(rooms = factor(rooms, levels = 1:6,
                            labels = paste0(1:6, " rooms")))
    plot_ly(d, x = ~rooms, y = ~price, type = "box",
            marker    = list(opacity = 0.4, size = 3),
            fillcolor = "rgba(33,150,243,0.4)",
            line      = list(color = "#1565C0")) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Price (TL)", tickformat = ",.0f"))
  })
  
  output$price_per_m2_hist <- renderPlotly({
    req(nrow(df()) > 0)
    d <- df() %>% filter(!is.na(price_per_m2))
    plot_ly(d, x = ~price_per_m2, type = "histogram",
            marker = list(color = "#FF9800"), nbinsx = 50) %>%
      layout(xaxis = list(title = "TL / m2", tickformat = ",.0f"),
             yaxis = list(title = "Count"))
  })
  
  output$correlation_heatmap <- renderPlotly({
    req(nrow(df()) > 0)
    cols <- intersect(
      c("price", "GrossSquareMeters", "NetSquareMeters",
        "rooms", "building_age", "bathroom_count", "price_per_m2"),
      names(df())
    )
    cm <- df() %>%
      select(all_of(cols)) %>%
      mutate(across(everything(), as.numeric)) %>%
      filter(if_all(everything(), ~ !is.na(.))) %>%
      cor()
    
    labels <- c(price = "Price", GrossSquareMeters = "Gross m2",
                NetSquareMeters = "Net m2", rooms = "Rooms",
                building_age = "Bldg Age", bathroom_count = "Bathrooms",
                price_per_m2 = "TL/m2")
    dn <- labels[colnames(cm)]
    dn[is.na(dn)] <- colnames(cm)[is.na(dn)]
    colnames(cm) <- rownames(cm) <- dn
    
    plot_ly(x = colnames(cm), y = rownames(cm), z = cm,
            type = "heatmap",
            colorscale   = list(c(0,"#E91E63"), c(.5,"white"), c(1,"#2196F3")),
            zmin = -1, zmax = 1,
            text = round(cm, 2), texttemplate = "%{text}") %>%
      layout(margin = list(l = 110, b = 110))
  })
  
  # ============================================================
  # 7. DISTRICT EXPLORER TAB
  # ============================================================
  dist_summary <- reactive({
    req(nrow(df()) > 0)
    df() %>%
      filter(!is.na(price), !is.na(district)) %>%
      group_by(district) %>%
      summarise(
        count        = n(),
        avg_price    = mean(price,             na.rm = TRUE),
        median_price = median(price,           na.rm = TRUE),
        avg_ppm2     = mean(price_per_m2,      na.rm = TRUE),
        avg_area     = mean(GrossSquareMeters, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(count >= 5) %>%
      arrange(desc(median_price))
  })
  
  output$district_chart <- renderPlotly({
    d <- dist_summary() %>% head(25)
    plot_ly(d,
            x = ~median_price,
            y = ~reorder(district, median_price),
            type = "bar", orientation = "h",
            marker       = list(color = "#1565C0", opacity = 0.85),
            text         = ~paste0(round(median_price / 1e6, 1), "M"),
            textposition = "outside") %>%
      layout(
        title  = list(text = "Median Sale Price by District (Top 25)",
                      font = list(size = 14)),
        xaxis  = list(title = "Median Price (TL)", tickformat = ",.0f"),
        yaxis  = list(title = ""),
        margin = list(l = 140, r = 90, t = 50)
      )
  })
  
  output$district_table <- renderDT({
    dist_summary() %>%
      mutate(
        avg_price    = format(round(avg_price),    big.mark = ","),
        median_price = format(round(median_price), big.mark = ","),
        avg_ppm2     = format(round(avg_ppm2),     big.mark = ","),
        avg_area     = round(avg_area, 0)
      ) %>%
      rename(
        District       = district,
        Listings       = count,
        "Avg Price"    = avg_price,
        "Median Price" = median_price,
        "Avg TL/m2"   = avg_ppm2,
        "Avg m2"       = avg_area
      ) %>%
      datatable(
        options  = list(pageLength = 20, scrollX = TRUE, dom = "tip"),
        rownames = FALSE,
        class    = "stripe hover compact"
      )
  })
  
  # ============================================================
  # 8. MODEL INFO TAB
  # ============================================================
  output$lm_summary <- renderPrint({
    m <- lm_model()
    if (is.null(m)) cat("Run 06_regression_model.R to train the model.")
    else            print(summary(m))
  })
  
  output$rf_importance <- renderPlotly({
    m <- rf_model()
    if (is.null(m))
      return(plot_ly() %>% layout(title = "Run 07_random_forest.R first."))
    
    imp <- as.data.frame(importance(m)) %>%
      tibble::rownames_to_column("feature") %>%
      arrange(`%IncMSE`)
    
    plot_ly(imp,
            x = ~`%IncMSE`, y = ~reorder(feature, `%IncMSE`),
            type = "bar", orientation = "h",
            marker = list(color = "#FF5722", opacity = 0.85)) %>%
      layout(xaxis  = list(title = "% Increase in MSE"),
             yaxis  = list(title = ""),
             margin = list(l = 160))
  })
  
  output$model_comparison_plot <- renderPlotly({
    lp <- here("models", "lm_metrics.rds")
    rp <- here("models", "rf_metrics.rds")
    if (!file.exists(lp) || !file.exists(rp))
      return(plot_ly() %>% layout(title = "Run scripts 06 and 07 first."))
    
    comp   <- bind_rows(readRDS(lp), readRDS(rp))
    colors <- c("Linear Regression" = "#2196F3", "Random Forest" = "#FF5722")
    
    make_bar <- function(metric_col, title_text) {
      plot_ly(comp,
              x      = ~model,
              y      = comp[[metric_col]],
              type   = "bar",
              color  = ~model,
              colors = colors,
              showlegend = (metric_col == "RMSE")) %>%
        layout(
          annotations = list(list(
            text      = title_text,
            x = 0.5, y = 1.08, xref = "paper", yref = "paper",
            showarrow = FALSE, font = list(size = 12)
          ))
        )
    }
    
    subplot(
      make_bar("RMSE", "RMSE (TL)"),
      make_bar("MAE",  "MAE (TL)"),
      make_bar("R2",   "R\u00b2"),
      nrows = 1, shareX = FALSE, shareY = FALSE
    ) %>%
      layout(
        title      = list(text = "Model Comparison", font = list(size = 14)),
        showlegend = TRUE,
        margin     = list(t = 65)
      )
  })
}