# ============================================================
# shiny-app/server.R  —  column names aligned with cleaning.py
# ============================================================

library(shiny)
library(dplyr)
library(plotly)
library(DT)
library(randomForest)
library(scales)
library(here)

server <- function(input, output, session) {
  
  # ── Load data & models ──────────────────────────────────────
  df <- reactive({
    p <- here("data", "processed", "all_listings.rds")
    if (file.exists(p)) readRDS(p) else data.frame()
  })
  
  rf_model <- reactive({
    p <- here("models", "random_forest_model.rds")
    if (file.exists(p)) readRDS(p) else NULL
  })
  
  rf_features <- reactive({
    p <- here("models", "rf_features.rds")
    if (file.exists(p)) readRDS(p)$features else
      c("GrossSquareMeters","room_count","numberOfBathrooms",
        "buildingAge","Elevator","Parking","InsideTheSite",
        "HeatingType","district")
  })
  
  lm_model <- reactive({
    p <- here("models", "linear_regression_model.rds")
    if (file.exists(p)) readRDS(p) else NULL
  })
  
  # ── Populate district dropdown ──────────────────────────────
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
                      choices = setNames(districts, districts),
                      selected = districts[1])
  })
  
  # ── Prediction ──────────────────────────────────────────────
  prediction <- eventReactive(input$predict_btn, {
    model <- rf_model()
    feats <- rf_features()
    
    new_apt <- data.frame(
      GrossSquareMeters = as.numeric(input$net_area),
      room_count        = as.numeric(input$room_count),
      numberOfBathrooms = as.numeric(input$bathroom_count),
      buildingAge       = as.numeric(input$building_age),
      Elevator          = as.logical(input$elevator),
      Parking           = as.logical(input$parking),
      InsideTheSite     = as.logical(input$within_site),
      HeatingType       = factor(input$heating_type),
      district          = factor(input$district)
    )
    
    if (is.null(model)) {
      return(list(price=NA, low=NA, high=NA, method="No model — run scripts 06 & 07"))
    }
    
    # Align factor levels to training data
    for (col in c("HeatingType", "district")) {
      lvls <- levels(model$forest$xlevels[[col]])
      if (!is.null(lvls))
        new_apt[[col]] <- factor(as.character(new_apt[[col]]),
                                 levels = union(levels(new_apt[[col]]), lvls))
    }
    
    log_pred <- tryCatch(
      predict(model, newdata = new_apt[, feats, drop=FALSE]),
      error = function(e) {
        cat("Prediction error:", conditionMessage(e), "\n")
        NA
      }
    )
    
    if (is.na(log_pred)) return(list(price=NA, low=NA, high=NA, method="Error"))
    
    price <- exp(log_pred)
    list(price=price, low=price*0.85, high=price*1.15, method="Random Forest")
  })
  
  # ── Similar apartments ──────────────────────────────────────
  similar <- eventReactive(input$predict_btn, {
    req(nrow(df()) > 0)
    df() %>%
      filter(
        listing_type == input$listing_type,
        district     == input$district,
        !is.na(price),
        !is.na(GrossSquareMeters),
        GrossSquareMeters >= input$net_area * 0.8,
        GrossSquareMeters <= input$net_area * 1.2
      ) %>%
      arrange(abs(GrossSquareMeters - input$net_area)) %>%
      head(50)
  })
  
  # ── UI outputs ───────────────────────────────────────────────
  output$price_display <- renderUI({
    pred <- prediction()
    fmt  <- function(x) format(round(x), big.mark=",")
    if (is.na(pred$price)) {
      return(div(class="price-display", pred$method))
    }
    unit <- if (input$listing_type == "satilik") "TL" else "TL / ay"
    div(
      div(class="price-display", paste0(fmt(pred$price), " ", unit)),
      div(class="price-range",
          sprintf("Tahmini aralık: %s – %s TL", fmt(pred$low), fmt(pred$high))),
      div(style="text-align:center;color:#999;font-size:.85em;margin-top:6px;",
          paste("Method:", pred$method))
    )
  })
  
  output$stat_avg_price <- renderUI({
    s <- similar(); avg <- if(nrow(s)>0) mean(s$price,na.rm=TRUE) else NA
    div(div(class="stat-value",
            if(is.na(avg)) "—" else format(round(avg), big.mark=",")),
        div(class="stat-label", "Ort. fiyat (ilçe)"))
  })
  
  output$stat_median_price <- renderUI({
    s <- similar(); med <- if(nrow(s)>0) median(s$price,na.rm=TRUE) else NA
    div(div(class="stat-value",
            if(is.na(med)) "—" else format(round(med), big.mark=",")),
        div(class="stat-label", "Medyan fiyat"))
  })
  
  output$stat_price_m2 <- renderUI({
    s <- similar(); pm2 <- if(nrow(s)>0) mean(s$price_per_m2,na.rm=TRUE) else NA
    div(div(class="stat-value",
            if(is.na(pm2)) "—" else format(round(pm2), big.mark=",")),
        div(class="stat-label", "Ort. TL / m²"))
  })
  
  output$similar_distribution <- renderPlotly({
    s <- similar(); pred <- prediction()
    if (nrow(s) == 0)
      return(plot_ly() %>% layout(title="Bu ilçede benzer ilan bulunamadı."))
    
    plot_ly(s, x=~price, type="histogram",
            marker=list(color="#2196F3",
                        line=list(color="white",width=0.5)),
            nbinsx=30, name="Benzer ilanlar") %>%
      add_lines(x=c(pred$price,pred$price), y=c(0,50),
                line=list(color="red",dash="dash",width=2),
                name="Tahmininiz") %>%
      layout(
        title=list(text="Benzer Dairelerin Fiyat Dağılımı", font=list(size=13)),
        xaxis=list(title="Fiyat (TL)", tickformat=",.0f"),
        yaxis=list(title="Sayı"),
        showlegend=TRUE, margin=list(t=40)
      )
  })
  
  output$similar_table <- renderDT({
    s <- similar()
    if (nrow(s) == 0) return(data.frame(Mesaj="Benzer ilan bulunamadı."))
    s %>%
      select(any_of(c("district","neighbourhood","price",
                      "GrossSquareMeters","NumberOfRooms",
                      "buildingAge","price_per_m2"))) %>%
      mutate(across(c(price, price_per_m2),
                    ~ format(round(as.numeric(.)), big.mark=","))) %>%
      rename_with(~ c("İlçe","Mahalle","Fiyat (TL)","Brüt m²",
                      "Oda","Bina Yaşı","TL/m²")[seq_along(.)]) %>%
      datatable(options=list(pageLength=8, scrollX=TRUE), rownames=FALSE)
  })
  
  # ── Market Statistics tab ───────────────────────────────────
  output$market_histogram <- renderPlotly({
    req(nrow(df())>0)
    d <- df() %>% filter(listing_type==input$stats_type, !is.na(price))
    plot_ly(d, x=~price, type="histogram",
            marker=list(color="#2196F3"), nbinsx=60) %>%
      layout(xaxis=list(title="Fiyat (TL)",tickformat=",.0f"),
             yaxis=list(title="Sayı"))
  })
  
  output$price_by_room <- renderPlotly({
    req(nrow(df())>0)
    d <- df() %>%
      filter(listing_type=="satilik",
             !is.na(NumberOfRooms), !is.na(price),
             NumberOfRooms %in% c("1+1","2+1","3+1","4+1","5+1"))
    plot_ly(d, x=~NumberOfRooms, y=~price, type="box",
            marker=list(opacity=0.5)) %>%
      layout(xaxis=list(title="Oda Sayısı"),
             yaxis=list(title="Fiyat (TL)",tickformat=",.0f"))
  })
  
  output$price_per_m2_hist <- renderPlotly({
    req(nrow(df())>0)
    d <- df() %>% filter(listing_type=="satilik", !is.na(price_per_m2))
    plot_ly(d, x=~price_per_m2, type="histogram",
            marker=list(color="#FF9800"), nbinsx=50) %>%
      layout(xaxis=list(title="TL / m²",tickformat=",.0f"),
             yaxis=list(title="Sayı"))
  })
  
  output$correlation_heatmap <- renderPlotly({
    req(nrow(df())>0)
    cols <- c("price","GrossSquareMeters","room_count",
              "buildingAge","numberOfBathrooms","price_per_m2")
    cols <- cols[cols %in% names(df())]
    cm <- df() %>%
      select(all_of(cols)) %>%
      mutate(across(everything(), as.numeric)) %>%
      filter(if_all(everything(), ~!is.na(.))) %>%
      cor()
    plot_ly(x=colnames(cm), y=rownames(cm), z=cm, type="heatmap",
            colorscale=list(c(0,"#E91E63"),c(.5,"white"),c(1,"#2196F3")),
            zmin=-1, zmax=1,
            text=round(cm,2), texttemplate="%{text}") %>%
      layout(margin=list(l=120,b=120))
  })
  
  # ── District Explorer tab ───────────────────────────────────
  dist_summary <- reactive({
    req(nrow(df())>0)
    df() %>%
      filter(listing_type==input$district_type,
             !is.na(price), !is.na(district)) %>%
      group_by(district) %>%
      summarise(
        count        = n(),
        avg_price    = mean(price,        na.rm=TRUE),
        median_price = median(price,      na.rm=TRUE),
        avg_price_m2 = mean(price_per_m2, na.rm=TRUE),
        .groups="drop"
      ) %>%
      filter(count >= 5) %>%
      arrange(desc(avg_price))
  })
  
  output$district_chart <- renderPlotly({
    d <- dist_summary() %>% head(20)
    plot_ly(d, x=~avg_price, y=~reorder(district,avg_price),
            type="bar", orientation="h",
            marker=list(color="#2196F3",opacity=0.8),
            text=~format(round(avg_price),big.mark=","),
            textposition="outside") %>%
      layout(xaxis=list(title="Ort. Fiyat (TL)",tickformat=",.0f"),
             yaxis=list(title=""), margin=list(l=130))
  })
  
  output$district_table <- renderDT({
    dist_summary() %>%
      mutate(across(c(avg_price,median_price,avg_price_m2),
                    ~format(round(as.numeric(.)),big.mark=","))) %>%
      rename(İlçe=district, Adet=count,
             "Ort. Fiyat"=avg_price, "Medyan Fiyat"=median_price,
             "Ort. TL/m²"=avg_price_m2) %>%
      datatable(options=list(pageLength=15), rownames=FALSE)
  })
  
  # ── Model Info tab ──────────────────────────────────────────
  output$lm_summary <- renderPrint({
    m <- lm_model()
    if (is.null(m)) cat("Modeli eğitmek için 06_regression_model.R çalıştırın.") else print(summary(m))
  })
  
  output$rf_importance <- renderPlotly({
    m <- rf_model()
    if (is.null(m)) return(plot_ly() %>% layout(title="Model henüz eğitilmedi."))
    imp <- as.data.frame(importance(m)) %>%
      tibble::rownames_to_column("feature") %>%
      arrange(`%IncMSE`)
    plot_ly(imp, x=~`%IncMSE`, y=~reorder(feature,`%IncMSE`),
            type="bar", orientation="h",
            marker=list(color="#FF5722")) %>%
      layout(xaxis=list(title="% MSE Artışı"),
             yaxis=list(title=""), margin=list(l=160))
  })
  
  output$model_comparison_plot <- renderPlotly({
    lp <- here("models","lm_metrics.rds")
    rp <- here("models","rf_metrics.rds")
    if (!file.exists(lp)||!file.exists(rp))
      return(plot_ly() %>%
               layout(title="Önce 06 ve 07 numaralı scriptleri çalıştırın."))
    comp <- bind_rows(readRDS(lp), readRDS(rp))
    plot_ly(comp, x=~model, y=~R2, type="bar",
            marker=list(color=c("#2196F3","#FF5722")),
            name="R²") %>%
      layout(title="Model R² Karşılaştırması",
             xaxis=list(title=""),
             yaxis=list(title="R² Skoru", range=c(0,1)))
  })
}