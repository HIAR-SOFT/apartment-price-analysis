# ============================================================
# 01_data_loading.R
# Istanbul Real-Estate — Sahibinden.com 2026 dataset
# Key columns from cleaned CSV:
#   listing_id | district | neighbourhood | price | price_per_m2
#   GrossSquareMeters | NetSquareMeters | rooms | halls | total_rooms
#   floor | floor_category | total_floors | building_age | building_type
#   building_condition | heating_type | fuel_type | bathroom_count
#   furnished | usage_status | is_in_complex | complex_name
#     maintenance_fee | orientation | credit_eligible | deed_status
# e    xchange | last_updated | scraped_at

library(readr)
library(dplyr)
library(here)

# cleaned .csv
raw_file <- list.files(
  here("data", "cleaned"),
  pattern = "cleaned_listings.*\\.csv$",
  full.names = TRUE
)[1]

if (is.na(raw_file) || !file.exists(raw_file)) {
  stop(
    "Cleaned dataset not found in data/cleaned/.\n",
    "Run prepare_data.py first:\n",
    "  python scraper/prepare_data.py --input data/raw/istanbul_apartment_prices_2026.csv"
  )
}
#column types 
cat("Loading:", raw_file, "\n")
raw <- read_csv(raw_file, show_col_types = FALSE)
cat("Raw shape:", nrow(raw), "x", ncol(raw), "\n")
cat("Raw columns:", paste(names(raw), collapse = ", "), "\n\n")

df <- raw

#dates

df <- df %>%
  mutate(
    last_updated = as.Date(last_updated),
    scraped_at   = as.POSIXct(scraped_at)
  )

# factor for building condition
df <- df %>%
  mutate(
    building_condition = factor(
      building_condition,
      levels = c("New", "Second-hand"),
      ordered = FALSE
    ),
    furnished = factor(
      furnished,
      levels = c("Furnished", "Unfurnished")
    ),
    credit_eligible = factor(credit_eligible)
  )

# subdistrict(mahalle)
df <- df %>%
  mutate(sub_district = neighbourhood)

cat("Sub-district populated:", sum(!is.na(df$sub_district)), "/", nrow(df), "\n")

# price_per_m2 from raw cols

df <- df %>%
  mutate(
    price_per_m2 = if_else(
      !is.na(GrossSquareMeters) & GrossSquareMeters > 0,
      price / GrossSquareMeters,
      NA_real_
    ),
    listing_type = "satilik"
  )

# Quality filter
# python already applied 1st–99th pct on price and 20–600 m² on gross area. We repeat some parts here so the R pipeline is able to run even if a raw file is loaded 
p01 <- quantile(df$price, 0.01, na.rm = TRUE)
p99 <- quantile(df$price, 0.99, na.rm = TRUE)

df_clean <- df %>%
  filter(
    !is.na(price),
    price >= p01, price <= p99,
    !is.na(GrossSquareMeters),
    GrossSquareMeters >= 20, GrossSquareMeters <= 500,
    !is.na(rooms)
  )

cat("\nAfter quality filter:", nrow(df_clean), "rows retained (from", nrow(df), ")\n")
cat("Removed:", nrow(df) - nrow(df_clean), "rows\n\n")

# ── 5. Summaries ──────────────────────────────────────────────
cat("=== PRICE SUMMARY (after filter) ===\n")
print(summary(df_clean$price))

cat("\n=== GROSS AREA SUMMARY ===\n")
print(summary(df_clean$GrossSquareMeters))

cat("\n=== NET AREA SUMMARY ===\n")
print(summary(df_clean$NetSquareMeters))

cat("\n=== ROOM COUNT ===\n")
print(table(df_clean$rooms))

cat("\n=== BUILDING CONDITION ===\n")
print(table(df_clean$building_condition, useNA = "ifany"))

cat("\n=== DISTRICT COUNTS ===\n")
print(sort(table(df_clean$district), decreasing = TRUE))

cat("\n=== MISSING VALUES ===\n")
mis <- sort(colSums(is.na(df_clean)), decreasing = TRUE)
print(mis[mis > 0])

# create directories, save 
for (d in c(
  here("data", "cleaned"),
  here("data", "processed"),
  here("models"),
  here("visuals", "histograms"),
  here("visuals", "boxplots"),
  here("visuals", "heatmaps"),
  here("visuals", "regression_plots")
)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

write_csv(df_clean, here("data", "cleaned", "cleaned_listings.csv"))
saveRDS(df_clean, here("data", "processed", "all_listings.rds"))

cat("\nSaved data/cleaned/cleaned_listings.csv\n")
cat(" Saved data/processed/all_listings.rds\n")
cat("Next: source('r-analysis/03_descriptive_statistics.R')\n")