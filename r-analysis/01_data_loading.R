# ============================================================
# 01_data_loading.R
# Istanbul Real-Estate — Sahibinden.com Kaggle dataset (May 2022)
# Columns in raw file:
#   Unnamed: 0 | title | area | numberOfRooms | price | town | district
# ============================================================

library(readr)
library(dplyr)
library(stringr)
library(here)

# ── 0. Locate CSV ─────────────────────────────────────────
raw_file <- list.files(
  here("data", "cleaned"),
  pattern = "cleaned_listings.*\\.csv$",
  full.names = TRUE
)[1]


if (!file.exists(raw_file)) {
  stop(
    "Raw dataset not found at: ", raw_file, "\n",
    "Place '22_5_2022_sahibinden_ev.csv' in data/raw/ and re-run."
  )
}

cat("Loading:", raw_file, "\n")
raw <- read_csv(raw_file, show_col_types = FALSE)
cat("Raw shape:", nrow(raw), "x", ncol(raw), "\n")
cat("Raw columns:", paste(names(raw), collapse = ", "), "\n\n")

# use cleaned dataset
df <- raw



# parse room count ───────────────────────────────────────
# Format: "3+1", "2+1", "Stüdyo", "4+2", ..
# room_count = sum of all parts (e.g. 3+1 → 4)
parse_rooms <- function(x) {
  x <- as.character(x)
  case_when(
    tolower(x) == "stüdyo" ~ 1,
    str_detect(x, "^[\\d.]+\\+[\\d.]+") ~ {
      parts <- str_split(x, "\\+")
      sapply(parts, function(p) sum(as.numeric(p), na.rm = TRUE))
    },
    TRUE ~ suppressWarnings(as.numeric(x))
  )
}

df <- df %>%
  mutate(room_count = parse_rooms(NumberOfRooms))

cat("Room count NA:", sum(is.na(df$room_count)), "\n")

# sub-district
df <- df %>%
  mutate(
    sub_district = neighbourhood
  )



cat("Sub-district extracted:", sum(!is.na(df$sub_district)), "/", nrow(df), "\n")

# ── 5. Derived features ───────────────────────────────────────
df <- df %>%
  mutate(
    price_per_m2 = if_else(
      !is.na(GrossSquareMeters) & GrossSquareMeters > 0,
      price / GrossSquareMeters,
      NA_real_
    ),
    # All listings in this dataset are for sale
    listing_type = "satilik"
  )

# ── 6. Basic quality filter (remove extreme outliers) ─────────
# Retain prices between 1st and 99th percentile; area 20–500 m²
p01  <- quantile(df$price, 0.01, na.rm = TRUE)
p99  <- quantile(df$price, 0.99, na.rm = TRUE)

df_clean <- df %>%
  filter(
    !is.na(price),
    price  >= p01,  price  <= p99,
    !is.na(GrossSquareMeters),
    GrossSquareMeters >= 20, GrossSquareMeters <= 500,
    !is.na(room_count)
  )

cat("\nAfter quality filter:", nrow(df_clean), "rows retained (from", nrow(df), ")\n")
cat("Removed:", nrow(df) - nrow(df_clean), "rows\n\n")

# ── 7. Summary ────────────────────────────────────────────────
cat("=== PRICE SUMMARY (after filter) ===\n")
print(summary(df_clean$price))

cat("\n=== AREA SUMMARY ===\n")
print(summary(df_clean$GrossSquareMeters))

cat("\n=== ROOM COUNT ===\n")
print(table(df_clean$NumberOfRooms))

cat("\n=== MISSING VALUES ===\n")
mis <- sort(colSums(is.na(df_clean)), decreasing = TRUE)
print(mis[mis > 0])

# ── 8. Create directories & save ──────────────────────────────
for (d in c(
  here("data", "cleaned"),
  here("data", "processed"),
  here("models"),
  here("visuals", "histograms"),
  here("visuals", "boxplots"),
  here("visuals", "heatmaps"),
  here("visuals", "regression_plots")
)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# Write cleaned CSV (for reference)
write_csv(df_clean, here("data", "cleaned", "cleaned_listings.csv"))

# Write .rds for downstream scripts
saveRDS(df_clean, here("data", "processed", "all_listings.rds"))

cat("\n✓ Saved data/cleaned/cleaned_listings.csv\n")
cat("✓ Saved data/processed/all_listings.rds\n")
cat("Next: source('r-analysis/03_descriptive_statistics.R')\n")