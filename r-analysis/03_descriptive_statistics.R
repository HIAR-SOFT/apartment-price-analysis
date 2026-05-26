# 03_descriptive_statistics.R

library(dplyr)
library(tidyr)
library(readr)
library(here)

df <- readRDS(here("data", "processed", "all_listings.rds"))

# Helper 
describe <- function(x, label = "value") {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (length(x) == 0)
    return(tibble(variable = label, n = 0L, mean = NA_real_,
                  median = NA_real_, sd = NA_real_, variance = NA_real_,
                  q25 = NA_real_, q75 = NA_real_,
                  min = NA_real_, max = NA_real_))
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

# Price stats 
price_stats <- bind_rows(
  describe(df$price,                                              "All prices (TL)"),
  describe(df %>% filter(listing_type == "satilik") %>% pull(price), "Sale prices (TL)"),
  describe(df %>% filter(listing_type == "kiralik") %>% pull(price), "Rent prices (TL)")
)
cat("=== PRICE STATS ===\n"); print(price_stats, width = 120)

# Area stats 
area_stats <- bind_rows(
  describe(df$HallSquareMeters,  "Net area (m²)"),
  describe(df$GrossSquareMeters, "Gross area (m²)")
)
cat("\n=== AREA STATS ===\n"); print(area_stats, width = 120)

# Price per m²
ppm2_stats <- describe(df$price_per_m2, "Price per m² (TL)")
cat("\n=== PRICE PER M² ===\n"); print(ppm2_stats, width = 120)

# Building age 
age_stats <- describe(df$buildingAge, "Building age (years)")
cat("\n=== BUILDING AGE ===\n"); print(age_stats, width = 120)

# District-level stats 
# Guard against missing district column
if ("district" %in% names(df)) {
  district_stats <- df %>%
    filter(!is.na(district), district != "NA") %>%
    group_by(district, listing_type) %>%
    summarise(
      count        = n(),
      avg_price    = mean(price,            na.rm = TRUE),
      median_price = median(price,          na.rm = TRUE),
      avg_price_m2 = mean(price_per_m2,     na.rm = TRUE),
      avg_gross_m2 = mean(GrossSquareMeters, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(avg_price))
  
  cat("\n=== TOP DISTRICTS BY AVG PRICE ===\n")
  print(head(district_stats, 20), width = 120)
} else {
  cat("\n[WARN] 'district' column not found — skipping district stats.\n")
  district_stats <- tibble()
}

#Room count distribution 
if ("NumberOfRooms" %in% names(df)) {
  room_dist <- df %>%
    filter(!is.na(NumberOfRooms), NumberOfRooms != "NA") %>%
    count(NumberOfRooms, listing_type) %>%
    arrange(listing_type, desc(n))
  cat("\n=== ROOM COUNT DISTRIBUTION ===\n"); print(room_dist)
} else {
  cat("\n[WARN] 'NumberOfRooms' column not found.\n")
  room_dist <- tibble()
}

# heating type distribution
if ("HeatingType" %in% names(df)) {
  heat_dist <- df %>%
    filter(!is.na(HeatingType), HeatingType != "NA") %>%
    count(HeatingType, sort = TRUE)
  cat("\n=== HEATING TYPE ===\n"); print(heat_dist)
}

# save summaries
vis_dir <- here("visuals")
dir.create(vis_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(price_stats,    file.path(vis_dir, "price_stats.csv"))
write_csv(area_stats,     file.path(vis_dir, "area_stats.csv"))
if (nrow(district_stats) > 0)
  write_csv(district_stats, file.path(vis_dir, "district_stats.csv"))
if (nrow(room_dist) > 0)
  write_csv(room_dist,      file.path(vis_dir, "room_distribution.csv"))

cat("\n✓ Descriptive stats saved to visuals/\n")
cat("Next: source('r-analysis/04_visualizations.R')\n")