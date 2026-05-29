# ============================================================
# 04_visualizations.R
# All plots for the Istanbul sale-price dataset
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

has_col <- function(col)
  col %in% names(df) && sum(!is.na(df[[col]])) > 0

# ── 1. Sale price histogram ───────────────────────────────────
if (has_col("price")) {
  p1 <- df %>%
    filter(!is.na(price)) %>%
    ggplot(aes(x = price / 1e6)) +
    geom_histogram(bins = 60, fill = "#2196F3", color = "white", linewidth = 0.2) +
    scale_x_continuous(labels = label_number(suffix = "M")) +
    labs(
      title    = "Distribution of Sale Prices — Istanbul (2022)",
      subtitle = "Prices in million TL  |  Source: Sahibinden.com / Kaggle",
      x = "Price (million TL)", y = "Count"
    ) + THEME
  save_plot(p1, "histograms/sale_price_histogram.png")

  # ── 2. Log-transformed price ──────────────────────────────
  p2 <- df %>%
    filter(!is.na(price), price > 0) %>%
    ggplot(aes(x = log(price))) +
    geom_histogram(bins = 50, fill = "#7C3AED", color = "white", linewidth = 0.2) +
    labs(
      title    = "Log-Transformed Sale Price Distribution",
      subtitle = "ln(Price) is approximately normal — justifies log regression",
      x = "ln(Price)", y = "Count"
    ) + THEME
  save_plot(p2, "histograms/log_price_histogram.png")
}

# ── 3. Gross area histogram ───────────────────────────────────
if (has_col("GrossSquareMeters")) {
  p3 <- df %>%
    filter(!is.na(GrossSquareMeters)) %>%
    ggplot(aes(x = GrossSquareMeters)) +
    geom_histogram(bins = 50, fill = "#FF9800", color = "white", linewidth = 0.2) +
    labs(
      title = "Gross Area Distribution — Istanbul Apartments",
      x = "Gross Area (m²)", y = "Count"
    ) + THEME
  save_plot(p3, "histograms/area_histogram.png")
}

# ── 4. Price vs Gross Area scatter ───────────────────────────
if (has_col("price") && has_col("GrossSquareMeters")) {
  p4 <- df %>%
    filter(!is.na(price), !is.na(GrossSquareMeters), price > 0) %>%
    ggplot(aes(x = GrossSquareMeters, y = price / 1e6)) +
    geom_point(alpha = 0.25, size = 0.7, color = "#2196F3") +
    geom_smooth(method = "lm", se = TRUE, color = "#E91E63") +
    scale_y_continuous(labels = label_number(suffix = "M")) +
    labs(
      title = "Sale Price vs Gross Area",
      x = "Gross Area (m²)", y = "Price (million TL)"
    ) + THEME
  save_plot(p4, "scatter_price_vs_area.png")
}

# ── 5. Price per m² histogram ─────────────────────────────────
if (has_col("price_per_m2")) {
  p5 <- df %>%
    filter(!is.na(price_per_m2),
           price_per_m2 < quantile(price_per_m2, 0.99, na.rm = TRUE)) %>%
    ggplot(aes(x = price_per_m2 / 1000)) +
    geom_histogram(bins = 50, fill = "#009688", color = "white", linewidth = 0.2) +
    scale_x_continuous(labels = label_number(suffix = "K")) +
    labs(
      title = "Price per m² Distribution",
      x = "TL per m² (thousands)", y = "Count"
    ) + THEME
  save_plot(p5, "histograms/price_per_m2_histogram.png")
}

# ── 6. Boxplot: price by room type ───────────────────────────
if (has_col("NumberOfRooms") && has_col("price")) {
  valid_rooms <- c("1+1", "2+1", "3+1", "4+1", "5+1", "3+2", "4+2")
  room_data <- df %>%
    filter(!is.na(NumberOfRooms), !is.na(price),
           as.character(NumberOfRooms) %in% valid_rooms)

  if (nrow(room_data) > 10) {
    p6 <- room_data %>%
      mutate(NumberOfRooms = factor(
        as.character(NumberOfRooms), levels = valid_rooms
      )) %>%
      ggplot(aes(x = NumberOfRooms, y = price / 1e6, fill = NumberOfRooms)) +
      geom_boxplot(alpha = 0.7, outlier.size = 0.5, show.legend = FALSE) +
      scale_y_continuous(labels = label_number(suffix = "M")) +
      scale_fill_brewer(palette = "Blues") +
      labs(
        title = "Sale Price by Room Configuration",
        x = "Room Type", y = "Price (million TL)"
      ) + THEME
    save_plot(p6, "boxplots/price_by_room_type.png")
  }
}

# ── 7. Boxplot: price by sub-district (top 15) ───────────────
if (has_col("sub_district") && has_col("price")) {
  top_districts <- df %>%
    filter(!is.na(sub_district)) %>%
    count(sub_district) %>%
    slice_max(n, n = 15) %>%
    pull(sub_district)

  if (length(top_districts) >= 2) {
    p7 <- df %>%
      filter(sub_district %in% top_districts, !is.na(price)) %>%
      mutate(sub_district = reorder(sub_district, price, median)) %>%
      ggplot(aes(x = sub_district, y = price / 1e6)) +
      geom_boxplot(fill = "#2196F3", alpha = 0.6, outlier.size = 0.5) +
      coord_flip() +
      scale_y_continuous(labels = label_number(suffix = "M")) +
      labs(
        title = "Sale Price by Sub-District — Top 15 by Listing Count",
        x = NULL, y = "Price (million TL)"
      ) + THEME
    save_plot(p7, "boxplots/sale_price_by_district.png", w = 10, h = 8)

    # ── 8. Price per m² by sub-district ──────────────────────
    if (has_col("price_per_m2")) {
      p8 <- df %>%
        filter(sub_district %in% top_districts, !is.na(price_per_m2)) %>%
        mutate(sub_district = reorder(sub_district, price_per_m2, median)) %>%
        ggplot(aes(x = sub_district, y = price_per_m2 / 1000)) +
        geom_boxplot(fill = "#FF9800", alpha = 0.6, outlier.size = 0.5) +
        coord_flip() +
        scale_y_continuous(labels = label_number(suffix = "K")) +
        labs(
          title = "Price per m² by Sub-District (Top 15)",
          x = NULL, y = "TL per m² (thousands)"
        ) + THEME
      save_plot(p8, "boxplots/price_per_m2_by_district.png", w = 10, h = 8)
    }
  }
}

# ── 9. Correlation heatmap ────────────────────────────────────
num_cols <- c("price", "GrossSquareMeters", "room_count", "price_per_m2")
num_cols <- num_cols[num_cols %in% names(df)]

cor_data <- df %>%
  select(all_of(num_cols)) %>%
  mutate(across(everything(), as.numeric)) %>%
  filter(if_all(everything(), ~ !is.na(.)))

if (nrow(cor_data) >= 10 && ncol(cor_data) >= 2) {
  cm    <- cor(cor_data)
  cm_df <- as.data.frame(as.table(cm))
  names(cm_df) <- c("Var1", "Var2", "value")

  col_labels <- c(
    price             = "Price (TL)",
    GrossSquareMeters = "Gross Area (m²)",
    room_count        = "Room Count",
    price_per_m2      = "Price / m²"
  )
  cm_df$Var1 <- recode(as.character(cm_df$Var1), !!!col_labels)
  cm_df$Var2 <- recode(as.character(cm_df$Var2), !!!col_labels)

  p9 <- ggplot(cm_df, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(value, 2)), size = 3.5) +
    scale_fill_gradient2(
      low = "#E91E63", mid = "white", high = "#2196F3",
      midpoint = 0, limits = c(-1, 1)
    ) +
    labs(
      title = "Pearson Correlation Heatmap — Apartment Attributes",
      x = NULL, y = NULL, fill = "r"
    ) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  save_plot(p9, "heatmaps/correlation_heatmap.png", w = 7, h = 6)
}

# ── 10. Room count bar chart (frequency) ─────────────────────
if (has_col("NumberOfRooms")) {
  top_rooms <- df %>%
    filter(!is.na(NumberOfRooms)) %>%
    count(NumberOfRooms) %>%
    slice_max(n, n = 12)

  p10 <- top_rooms %>%
    mutate(NumberOfRooms = reorder(NumberOfRooms, n)) %>%
    ggplot(aes(x = NumberOfRooms, y = n, fill = n)) +
    geom_col(show.legend = FALSE) +
    scale_fill_gradient(low = "#BBDEFB", high = "#1565C0") +
    coord_flip() +
    labs(
      title = "Listing Count by Room Configuration",
      x = "Room Type", y = "Number of Listings"
    ) + THEME
  save_plot(p10, "room_count_barchart.png", w = 8, h = 5)
}

cat("\n✓ All visualizations saved to visuals/\n")
cat("Next: source('r-analysis/05_hypothesis_testing.R')\n")