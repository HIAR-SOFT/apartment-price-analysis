
# 03_descriptive_statistics.R


library(dplyr)
library(tidyr)
library(readr)
library(here)

df <- readRDS(here("data", "processed", "all_listings.rds"))

# ── Helper: one-variable summary ─────────────────────────────
describe <- function(x, label = "value") {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (length(x) == 0)
    return(tibble(
      variable = label, n = 0L,
      mean = NA_real_, median = NA_real_, sd = NA_real_,
      variance = NA_real_, q25 = NA_real_, q75 = NA_real_,
      min = NA_real_, max = NA_real_, skewness = NA_real_,
      kurtosis = NA_real_
    ))
  
  # excess skewness and kurtosis
  n    <- length(x)
  m    <- mean(x)
  s    <- sd(x)
  sk   <- (n / ((n - 1) * (n - 2))) * sum(((x - m) / s)^3)
  kurt <- ((n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))) *
    sum(((x - m) / s)^4) -
    (3 * (n - 1)^2) / ((n - 2) * (n - 3))
  
  tibble(
    variable = label, n = n,
    mean     = m,
    median   = median(x),
    sd       = s,
    variance = var(x),
    q25      = quantile(x, 0.25),
    q75      = quantile(x, 0.75),
    min      = min(x),
    max      = max(x),
    skewness = sk,
    kurtosis = kurt
  )
}

# ── 1. Price stats ────────────────────────────────────────────
price_stats <- describe(df$price, "Sale price (TL)")
cat("=== PRICE STATISTICS ===\n")
print(price_stats, width = 120)

# ── 2. Gross area stats ───────────────────────────────────────
area_stats <- describe(df$GrossSquareMeters, "Gross area (m²)")
cat("\n=== GROSS AREA STATISTICS ===\n")
print(area_stats, width = 120)

# ── 3. Net area stats ─────────────────────────────────────────
net_area_stats <- describe(df$NetSquareMeters, "Net area (m²)")
cat("\n=== NET AREA STATISTICS ===\n")
print(net_area_stats, width = 120)

# -- 4. Price per m2 (NET-based -- primary metric) ------------
# price_per_m2 = price / net_sqm — the economically correct basis.
# Gross-based price/m² artificially deflates older inner-city districts
# (Kagithane, Fatih, Beyoglu) by 30-45% vs new outer suburbs because
# their large common areas inflate gross area far more than in modern
# new-build districts.  This is the root cause of the district ranking
# mismatch with market reality.
ppm2_stats <- describe(df$price_per_m2, "Price per NET m2 (TL)")
cat("\n=== PRICE PER NET M2 (primary metric) ===\n")
print(ppm2_stats, width = 120)

# Also report gross-based for reference
if ("price_per_gross_m2" %in% names(df)) {
  ppm2_gross_stats <- describe(df$price_per_gross_m2, "Price per GROSS m2 (TL)")
  cat("\n=== PRICE PER GROSS M2 (reference only — do NOT use for district comparisons) ===\n")
  print(ppm2_gross_stats, width = 120)
}

# ── 5. Room count distribution ────────────────────────────────
# rooms is now a plain integer column — no string parsing needed
cat("\n=== ROOM COUNT DISTRIBUTION ===\n")
room_dist <- df %>%
  filter(!is.na(rooms)) %>%
  count(rooms, name = "count") %>%
  mutate(pct = round(100 * count / sum(count), 1)) %>%
  arrange(rooms)
print(room_dist, n = 30)

# ── 6. Building condition breakdown ──────────────────────────
cat("\n=== BUILDING CONDITION ===\n")
print(table(df$building_condition, useNA = "ifany"))

# ── 7. Furnished status breakdown ────────────────────────────
cat("\n=== FURNISHED STATUS ===\n")
print(table(df$furnished, useNA = "ifany"))

# ── 8. Sub-district level stats ───────────────────────────────
if ("sub_district" %in% names(df)) {
  district_stats <- df %>%
    filter(!is.na(sub_district)) %>%
    group_by(sub_district) %>%
    summarise(
      count        = n(),
      avg_price    = mean(price,            na.rm = TRUE),
      median_price = median(price,          na.rm = TRUE),
      avg_ppm2     = mean(price_per_m2,     na.rm = TRUE),
      avg_gross_m2 = mean(GrossSquareMeters, na.rm = TRUE),
      avg_net_m2   = mean(NetSquareMeters,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(median_price))
  
  cat("\n=== TOP 20 SUB-DISTRICTS BY MEDIAN PRICE ===\n")
  print(head(district_stats, 20), width = 120)
} else {
  district_stats <- tibble()
  cat("\n[WARN] sub_district column not found.\n")
}

# ── 9. Coefficient of Variation (CV) ─────────────────────────
cv_price <- (sd(df$price,             na.rm = TRUE) / mean(df$price,             na.rm = TRUE)) * 100
cv_area  <- (sd(df$GrossSquareMeters, na.rm = TRUE) / mean(df$GrossSquareMeters, na.rm = TRUE)) * 100

cat("\n=== COEFFICIENT OF VARIATION ===\n")
cat(sprintf("Price CV : %.1f%%\n", cv_price))
cat(sprintf("Area  CV : %.1f%%\n", cv_area))

# ── 10. IQR and Outlier counts ───────────────────────────────
iqr_price <- IQR(df$price, na.rm = TRUE)
fence_lo  <- quantile(df$price, 0.25, na.rm = TRUE) - 1.5 * iqr_price
fence_hi  <- quantile(df$price, 0.75, na.rm = TRUE) + 1.5 * iqr_price
n_outliers <- sum(df$price < fence_lo | df$price > fence_hi, na.rm = TRUE)

cat("\n=== TUKEY OUTLIER FENCES (Price) ===\n")
cat(sprintf("IQR         : %s TL\n", format(round(iqr_price), big.mark = ",")))
cat(sprintf("Lower fence : %s TL\n", format(round(fence_lo),  big.mark = ",")))
cat(sprintf("Upper fence : %s TL\n", format(round(fence_hi),  big.mark = ",")))
cat(sprintf("Outliers    : %d (%.1f%%)\n",
            n_outliers, 100 * n_outliers / nrow(df)))

# ── 11. Save summaries ────────────────────────────────────────
vis_dir <- here("visuals")
dir.create(vis_dir, recursive = TRUE, showWarnings = FALSE)

all_stats <- bind_rows(price_stats, area_stats, net_area_stats, ppm2_stats)
write_csv(all_stats,    here("visuals", "price_area_stats.csv"))
write_csv(room_dist,    here("visuals", "room_distribution.csv"))
if (nrow(district_stats) > 0)
  write_csv(district_stats, here("visuals", "district_stats.csv"))

cat("\n✓ Descriptive stats saved to visuals/\n")
cat("Next: source('r-analysis/04_visualizations.R')\n")