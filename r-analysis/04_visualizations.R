# ============================================================
# 04_visualizations.R
# Generates all ggplot2 charts and saves them to visuals/.
# ============================================================

library(dplyr)
library(ggplot2)
library(scales)
library(corrplot)

# ── Load ──────────────────────────────────────────────────────
df <- readRDS("data/processed/all_listings.rds")

THEME <- theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray50"),
    panel.grid.minor = element_blank()
  )

save_plot <- function(p, filename, w = 10, h = 6) {
  path <- file.path("visuals", filename)
  ggsave(path, plot = p, width = w, height = h, dpi = 150)
  cat("Saved:", path, "\n")
}

# ── 1. Price histograms ───────────────────────────────────────
p1a <- df %>%
  filter(listing_type == "satilik", !is.na(price)) %>%
  ggplot(aes(x = price / 1e6)) +
  geom_histogram(bins = 60, fill = "#2196F3", color = "white", linewidth = 0.2) +
  scale_x_continuous(labels = label_number(suffix = "M")) +
  labs(
    title    = "Distribution of Sale Prices — Istanbul",
    subtitle = "Prices in million TL",
    x = "Price (million TL)", y = "Count"
  ) + THEME
save_plot(p1a, "histograms/sale_price_histogram.png")

p1b <- df %>%
  filter(listing_type == "kiralik", !is.na(price)) %>%
  ggplot(aes(x = price / 1000)) +
  geom_histogram(bins = 60, fill = "#4CAF50", color = "white", linewidth = 0.2) +
  scale_x_continuous(labels = label_number(suffix = "K")) +
  labs(
    title    = "Distribution of Rental Prices — Istanbul",
    subtitle = "Monthly rent in thousand TL",
    x = "Monthly Rent (thousand TL)", y = "Count"
  ) + THEME
save_plot(p1b, "histograms/rent_price_histogram.png")

# ── 2. Log-transformed price ──────────────────────────────────
p2 <- df %>%
  filter(!is.na(price)) %>%
  ggplot(aes(x = log(price), fill = listing_type)) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c(satilik = "#2196F3", kiralik = "#4CAF50"),
                    labels = c("For Sale", "For Rent")) +
  labs(
    title = "Log-Transformed Price Distribution",
    x = "ln(Price)", y = "Count", fill = NULL
  ) + THEME
save_plot(p2, "histograms/log_price_distribution.png")

# ── 3. Apartment area histogram ───────────────────────────────
p3 <- df %>%
  filter(!is.na(net_area_m2), net_area_m2 < 400) %>%
  ggplot(aes(x = net_area_m2)) +
  geom_histogram(bins = 50, fill = "#FF9800", color = "white", linewidth = 0.2) +
  labs(
    title = "Net Area Distribution — Istanbul Apartments",
    x = "Net Area (m²)", y = "Count"
  ) + THEME
save_plot(p3, "histograms/area_histogram.png")

# ── 4. Price vs Area scatter plot ─────────────────────────────
p4 <- df %>%
  filter(!is.na(price), !is.na(net_area_m2), net_area_m2 < 400) %>%
  ggplot(aes(x = net_area_m2, y = price / 1e6, color = listing_type)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c(satilik = "#2196F3", kiralik = "#E91E63"),
                     labels = c("For Sale", "For Rent")) +
  scale_y_continuous(labels = label_number(suffix = "M")) +
  labs(
    title    = "Price vs Net Area",
    subtitle = "Linear trend fitted per listing type",
    x = "Net Area (m²)", y = "Price (million TL)", color = NULL
  ) + THEME
save_plot(p4, "scatter_price_vs_area.png")

# ── 5. District price boxplots ────────────────────────────────
top_districts <- df %>%
  count(district) %>%
  top_n(15, n) %>%
  pull(district)

p5 <- df %>%
  filter(listing_type == "satilik", district %in% top_districts, !is.na(price)) %>%
  mutate(district = reorder(district, price, median)) %>%
  ggplot(aes(x = district, y = price / 1e6)) +
  geom_boxplot(fill = "#2196F3", alpha = 0.6, outlier.size = 0.5) +
  coord_flip() +
  scale_y_continuous(labels = label_number(suffix = "M")) +
  labs(
    title = "Sale Price by District — Top 15 Districts",
    x = NULL, y = "Price (million TL)"
  ) + THEME
save_plot(p5, "boxplots/sale_price_by_district.png", w = 10, h = 8)

# ── 6. Room count price boxplot ───────────────────────────────
p6 <- df %>%
  filter(listing_type == "satilik", !is.na(room_count_label), !is.na(price)) %>%
  filter(room_count_label %in% c("1+1", "2+1", "3+1", "4+1", "5+1")) %>%
  ggplot(aes(x = room_count_label, y = price / 1e6, fill = room_count_label)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.5, show.legend = FALSE) +
  scale_y_continuous(labels = label_number(suffix = "M")) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Sale Price by Room Count",
    x = "Room Count", y = "Price (million TL)"
  ) + THEME
save_plot(p6, "boxplots/price_by_room_count.png")

# ── 7. Correlation heatmap ────────────────────────────────────
num_cols <- c("price", "net_area_m2", "gross_area_m2", "room_count",
              "floor_number", "building_age", "bathroom_count", "price_per_m2")

cor_data <- df %>%
  select(all_of(num_cols)) %>%
  filter(if_all(everything(), ~ !is.na(.))) %>%
  cor()

png("visuals/heatmaps/correlation_heatmap.png", width = 900, height = 800, res = 130)
corrplot(cor_data,
  method = "color", type = "upper",
  tl.cex = 0.9, tl.col = "black",
  addCoef.col = "black", number.cex = 0.75,
  col = colorRampPalette(c("#E91E63", "white", "#2196F3"))(100),
  title = "Correlation Heatmap — Apartment Attributes",
  mar = c(0, 0, 2, 0)
)
dev.off()
cat("Saved: visuals/heatmaps/correlation_heatmap.png\n")

# ── 8. Price per m² by district ───────────────────────────────
p8 <- df %>%
  filter(listing_type == "satilik", district %in% top_districts, !is.na(price_per_m2)) %>%
  mutate(district = reorder(district, price_per_m2, median)) %>%
  ggplot(aes(x = district, y = price_per_m2)) +
  geom_boxplot(fill = "#FF9800", alpha = 0.6, outlier.size = 0.5) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Price per m² by District (For Sale)",
    x = NULL, y = "TL per m²"
  ) + THEME
save_plot(p8, "boxplots/price_per_m2_by_district.png", w = 10, h = 8)

cat("\n✓ All visualizations saved to visuals/\n")