# ============================================================
# 01_data_loading.R
# Loads cleaned CSV (column names match PREPROCESS_FULL.ipynb)
# into R, sets correct types, saves .rds files.
# ============================================================

library(readr)
library(dplyr)
library(lubridate)
library(here)

# ── 1. Find the most recent cleaned CSV ──────────────────────
cleaned_dir <- here("data", "cleaned")
csv_files   <- list.files(cleaned_dir, pattern = "cleaned_listings.*\\.csv$", full.names = TRUE)

if (length(csv_files) == 0) {
  stop(
    "No cleaned CSV found in data/cleaned/\n",
    "Run:  python scraper/scraper.py\n",
    "Then: python scraper/parser.py --input data/raw/listing_links_XXX.json\n",
    "Then: python scraper/cleaning.py --input data/raw/raw_listings_XXX.csv"
  )
}

csv_file <- csv_files[length(csv_files)]   # most recent
cat("Loading:", csv_file, "\n")

# ── 2. Read ───────────────────────────────────────────────────
df <- read_csv(csv_file, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
cat("Shape:", nrow(df), "x", ncol(df), "\n")
cat("Columns:", paste(names(df), collapse = ", "), "\n\n")

# ── 3. Type coercions ────────────────────────────────────────
# Categorical
cat_cols <- c(
  "listing_type", "district", "neighbourhood",
  "UsingStatus", "BuildStatus", "TitleStatus", "HeatingType",
  "StructureType", "BalconyType", "EligibilityForInvestment",
  "ItemStatus", "CreditEligibility", "InsideTheSite",
  "MortgageStatus", "Swap", "Balcony", "IsItVideoNavigable?",
  "NumberOfRooms", "FloorLocation", "KitchenType",
  "Elevator", "Parking"
)
for (col in cat_cols) {
  if (col %in% names(df)) df[[col]] <- as.factor(df[[col]])
}

# Binary logical
bin_cols <- c("Elevator", "Parking", "InsideTheSite",
              "CreditEligibility", "EligibilityForInvestment")
for (col in bin_cols) {
  if (col %in% names(df)) {
    df[[col]] <- df[[col]] %in% c("Var", "Evet", "var", "evet", "TRUE", "1")
  }
}

# Numeric
num_cols <- c(
  "price", "GrossSquareMeters", "HallSquareMeters",
  "buildingAge", "numberOfBathrooms", "numberOfBalconies",
  "numberFloorsOfBuilding", "rentalIncome", "subscription",
  "adUpdateMonth", "adUpdateYear", "adActiveDays",
  "room_count", "price_per_m2",
  "BalconySquareMeters", "WCSquareMeters"
)
for (col in num_cols) {
  if (col %in% names(df)) df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
}

# ── 4. Derived columns (if not already present) ───────────────
if (!"price_per_m2" %in% names(df) && "price" %in% names(df) && "GrossSquareMeters" %in% names(df)) {
  df <- df %>%
    mutate(price_per_m2 = ifelse(GrossSquareMeters > 0, price / GrossSquareMeters, NA_real_))
}

# ── 5. Split sale / rent ──────────────────────────────────────
df_satilik <- df %>% filter(listing_type == "satilik")
df_kiralik <- df %>% filter(listing_type == "kiralik")

cat("Sale listings :", nrow(df_satilik), "\n")
cat("Rent listings :", nrow(df_kiralik), "\n")

# ── 6. Quick summary ─────────────────────────────────────────
cat("\nPrice summary (all listings):\n")
print(summary(df$price))
cat("\nMissing values (top 10):\n")
mis <- sort(colSums(is.na(df)), decreasing = TRUE)
print(head(mis[mis > 0], 10))

# ── 7. Save .rds ─────────────────────────────────────────────
processed_dir <- here("data", "processed")
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(df,         file.path(processed_dir, "all_listings.rds"))
saveRDS(df_satilik, file.path(processed_dir, "satilik.rds"))
saveRDS(df_kiralik, file.path(processed_dir, "kiralik.rds"))

cat("\n✓  .rds files saved to data/processed/\n")
cat("Next: source('r-analysis/03_descriptive_statistics.R')\n")