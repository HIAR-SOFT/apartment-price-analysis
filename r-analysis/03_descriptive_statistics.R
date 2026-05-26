# ============================================================
# 03_descriptive_statistics.R
# Calculates mean, median, variance, std dev, quartiles,
# min/max for prices, areas, and per-m2 prices.
# Saves summary tables to visuals/.
# ============================================================

library(dplyr)
library(tidyr)
library(readr)

# ── Load ──────────────────────────────────────────────────────
df <- readRDS("data/processed/all_listings.rds")

# ── Helper: descriptive stats for one numeric column ─────────
describe <- function(x, label = "value") {
  x <- x[!is.na(x)]
  tibble(
    variable = label,
    n        = length(x),
    mean     = mean(x),
    median   = median(x),
    sd       = sd(x),
    variance = var(x),
    q25      = quantile(x, 0.25),
    q75      = quantile(x, 0.75),
    min      = min(x),
    max      = max(x)
  )
}

# ── 1. Overall price stats ────────────────────────────────────
price_stats <- bind_rows(
  describe(df$price,        "All prices (TL)"),
  describe(df %>% filter(listing_type == "satilik") %>% pull(price), "Sale prices (TL)"),
  describe(df %>% filter(listing_type == "kiralik") %>% pull(price), "Rent prices (TL)")
)

print("=== PRICE STATS ===")
print(price_stats)

# ── 2. Area stats ─────────────────────────────────────────────
area_stats <- bind_rows(
  describe(df$net_area_m2,   "Net area (m²)"),
  describe(df$gross_area_m2, "Gross area (m²)")
)
print("=== AREA STATS ===")
print(area_stats)

# ── 3. Price per m² ──────────────────────────────────────────
ppm2_stats <- describe(df$price_per_m2, "Price per m² (TL)")
print("=== PRICE PER M² ===")
print(ppm2_stats)

# ── 4. District-level stats ───────────────────────────────────
district_stats <- df %>%
  group_by(district, listing_type) %>%
  summarise(
    count        = n(),
    avg_price    = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    avg_price_m2 = mean(price_per_m2, na.rm = TRUE),
    avg_area     = mean(net_area_m2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_price))

print("=== TOP DISTRICTS BY AVG PRICE ===")
print(head(district_stats, 20))

# ── 5. Room count distribution ───────────────────────────────
room_dist <- df %>%
  count(room_count_label, listing_type) %>%
  arrange(listing_type, desc(n))

print("=== ROOM COUNT DISTRIBUTION ===")
print(room_dist)

# ── 6. Save summaries ────────────────────────────────────────
write_csv(price_stats,    "visuals/price_stats.csv")
write_csv(area_stats,     "visuals/area_stats.csv")
write_csv(district_stats, "visuals/district_stats.csv")
write_csv(room_dist,      "visuals/room_distribution.csv")

cat("\n✓ Descriptive stats saved to visuals/\n")