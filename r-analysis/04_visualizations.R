# ============================================================
# 04_visualizations.R
# ============================================================

library(dplyr)
library(ggplot2)
library(scales)
library(here)

df <- readRDS(here("data", "processed", "all_listings.rds"))

THEME <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(color = "gray50"),
    panel.grid.minor = element_blank()
  )

save_plot <- function(p, path, w = 10, h = 6) {
  full <- here("visuals", path)
  dir.create(dirname(full), recursive = TRUE, showWarnings = FALSE)
  ggsave(full, plot = p, width = w, height = h, dpi = 150)
  cat("Saved:", full, "\n")
}

# Guard: check key columns exist and have data
has_col <- function(col) col %in% names(df) && sum(!is.na(df[[col]])) > 0

# ── 1. Sale price histogram ───────────────────────────────────
if (has_col("price")) {
  p1a <- df %>%
    filter(listing_type == "satilik", !is.na(price)) %>%
    ggplot(aes(x = price / 1e6)) +
    geom_histogram(bins = 60, fill = "#2196F3", color = "white", linewidth = 0.2) +
    scale_x_continuous(labels = label_number(suffix = "M")) +
    labs(title = "Distribution of Sale Prices — Istanbul",
         subtitle = "Prices in million TL",
         x = "Price (million TL)", y = "Count") + THEME
  save_plot(p1a, "histograms/sale_price_histogram.png")
  
  # ── 2. Rent price histogram ─────────────────────────────────
  p1b <- df %>%
    filter(listing_type == "kiralik", !is.na(price)) %>%
    ggplot(aes(x = price / 1000)) +
    geom_histogram(bins = 60, fill = "#4CAF50", color = "white", linewidth = 0.2) +
    scale_x_continuous(labels = label_number(suffix = "K")) +
    labs(title = "Distribution of Rental Prices — Istanbul",
         subtitle = "Monthly rent in thousand TL",
         x = "Monthly Rent (thousand TL)", y = "Count") + THEME
  save_plot(p1b, "histograms/rent_price_histogram.png")
  
  # ── 3. Log-transformed price ──────────────────────────────
  p2 <- df %>%
    filter(!is.na(price), price > 0) %>%
    ggplot(aes(x = log(price), fill = listing_type)) +
    geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
    scale_fill_manual(values = c(satilik = "#2196F3", kiralik = "#4CAF50"),
                      labels = c("For Sale", "For Rent")) +
    labs(title = "Log-Transformed Price Distribution",
         x = "ln(Price)", y = "Count", fill = NULL) + THEME
  save_plot(p2, "histograms/log_price_distribution.png")
}

# ── 4. Gross area histogram ───────────────────────────────────
if (has_col("GrossSquareMeters")) {
  p3 <- df %>%
    filter(!is.na(GrossSquareMeters), GrossSquareMeters < 400) %>%
    ggplot(aes(x = GrossSquareMeters)) +
    geom_histogram(bins = 50, fill = "#FF9800", color = "white", linewidth = 0.2) +
    labs(title = "Gross Area Distribution — Istanbul Apartments",
         x = "Gross Area (m²)", y = "Count") + THEME
  save_plot(p3, "histograms/area_histogram.png")
}

# ── 5. Price vs GrossSquareMeters scatter ─────────────────────
if (has_col("price") && has_col("GrossSquareMeters")) {
  p4 <- df %>%
    filter(!is.na(price), !is.na(GrossSquareMeters),
           GrossSquareMeters < 400, price > 0) %>%
    ggplot(aes(x = GrossSquareMeters, y = price / 1e6, color = listing_type)) +
    geom_point(alpha = 0.3, size = 0.8) +
    geom_smooth(method = "lm", se = TRUE) +
    scale_color_manual(values = c(satilik = "#2196F3", kiralik = "#E91E63"),
                       labels = c("For Sale", "For Rent")) +
    scale_y_continuous(labels = label_number(suffix = "M")) +
    labs(title = "Price vs Gross Area",
         x = "Gross Area (m²)", y = "Price (million TL)", color = NULL) + THEME
  save_plot(p4, "scatter_price_vs_area.png")
}

# ── 6. District price boxplots ────────────────────────────────
if (has_col("district") && has_col("price")) {
  top_districts <- df %>%
    filter(!is.na(district), district != "NA") %>%
    count(district) %>%
    slice_max(n, n = 15) %>%
    pull(district)
  
  if (length(top_districts) > 0) {
    p5 <- df %>%
      filter(listing_type == "satilik",
             district %in% top_districts, !is.na(price)) %>%
      mutate(district = reorder(district, price, median)) %>%
      ggplot(aes(x = district, y = price / 1e6)) +
      geom_boxplot(fill = "#2196F3", alpha = 0.6, outlier.size = 0.5) +
      coord_flip() +
      scale_y_continuous(labels = label_number(suffix = "M")) +
      labs(title = "Sale Price by District — Top 15",
           x = NULL, y = "Price (million TL)") + THEME
    save_plot(p5, "boxplots/sale_price_by_district.png", w = 10, h = 8)
    
    # ── 9. Price per m² by district ──────────────────────────
    if (has_col("price_per_m2")) {
      p8 <- df %>%
        filter(listing_type == "satilik",
               district %in% top_districts, !is.na(price_per_m2)) %>%
        mutate(district = reorder(district, price_per_m2, median)) %>%
        ggplot(aes(x = district, y = price_per_m2)) +
        geom_boxplot(fill = "#FF9800", alpha = 0.6, outlier.size = 0.5) +
        coord_flip() +
        scale_y_continuous(labels = comma) +
        labs(title = "Price per m² by District (For Sale)",
             x = NULL, y = "TL per m²") + THEME
      save_plot(p8, "boxplots/price_per_m2_by_district.png", w = 10, h = 8)
    }
  }
}

# ── 7. Price by room count ────────────────────────────────────
if (has_col("NumberOfRooms") && has_col("price")) {
  valid_rooms <- c("1+1", "2+1", "3+1", "4+1", "5+1")
  room_data <- df %>%
    filter(listing_type == "satilik",
           !is.na(NumberOfRooms), !is.na(price),
           as.character(NumberOfRooms) %in% valid_rooms)
  
  if (nrow(room_data) > 0) {
    p6 <- room_data %>%
      mutate(NumberOfRooms = factor(as.character(NumberOfRooms),
                                    levels = valid_rooms)) %>%
      ggplot(aes(x = NumberOfRooms, y = price / 1e6, fill = NumberOfRooms)) +
      geom_boxplot(alpha = 0.7, outlier.size = 0.5, show.legend = FALSE) +
      scale_y_continuous(labels = label_number(suffix = "M")) +
      scale_fill_brewer(palette = "Blues") +
      labs(title = "Sale Price by Room Count",
           x = "Room Count", y = "Price (million TL)") + THEME
    save_plot(p6, "boxplots/price_by_room_count.png")
  }
}

# ── 8. Correlation heatmap ────────────────────────────────────
num_for_cor <- c("price", "GrossSquareMeters", "HallSquareMeters",
                 "room_count", "buildingAge", "numberOfBathrooms",
                 "price_per_m2", "numberFloorsOfBuilding")
num_for_cor <- num_for_cor[num_for_cor %in% names(df)]

cor_data <- df %>%
  select(all_of(num_for_cor)) %>%
  mutate(across(everything(), as.numeric)) %>%
  filter(if_all(everything(), ~ !is.na(.)))

if (nrow(cor_data) >= 10 && ncol(cor_data) >= 2) {
  # Use ggplot instead of corrplot to avoid extra dependency issues
  cm     <- cor(cor_data)
  cm_df  <- as.data.frame(as.table(cm))
  names(cm_df) <- c("Var1", "Var2", "value")
  
  p_cor <- ggplot(cm_df, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(value, 2)), size = 3) +
    scale_fill_gradient2(low = "#E91E63", mid = "white", high = "#2196F3",
                         midpoint = 0, limits = c(-1, 1)) +
    labs(title = "Correlation Heatmap — Apartment Attributes",
         x = NULL, y = NULL, fill = "r") +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  save_plot(p_cor, "heatmaps/correlation_heatmap.png", w = 9, h = 8)
} else {
  cat("[WARN] Not enough complete rows for correlation heatmap.\n")
}

# ── 10. Building age vs price ─────────────────────────────────
if (has_col("buildingAge") && has_col("price")) {
  age_data <- df %>%
    filter(listing_type == "satilik",
           !is.na(buildingAge), !is.na(price), buildingAge <= 50)
  
  if (nrow(age_data) > 5) {
    p9 <- age_data %>%
      ggplot(aes(x = buildingAge, y = price / 1e6)) +
      geom_point(alpha = 0.25, size = 0.8, color = "#9C27B0") +
      geom_smooth(method = "loess", se = TRUE, color = "#7B1FA2") +
      scale_y_continuous(labels = label_number(suffix = "M")) +
      labs(title = "Sale Price vs Building Age",
           x = "Building Age (years)", y = "Price (million TL)") + THEME
    save_plot(p9, "scatter_price_vs_age.png")
  }
}

cat("\n✓ All visualizations saved to visuals/\n")
cat("Next: source('r-analysis/05_hypothesis_testing.R')\n")