# 01_data_loading.R


library(readr)
library(dplyr)
library(here)

# Find most recent cleaned CSV
cleaned_dir <- here("data", "cleaned")
csv_files   <- list.files(cleaned_dir,
                          pattern = "cleaned_listings.*\\.csv$",
                          full.names = TRUE)

if (length(csv_files) == 0) {
  stop(
    "No cleaned CSV in data/cleaned/\n",
    "Run:\n",
    "  python scraper/scraper.py\n",
    "  python scraper/parser.py  --input data/raw/listing_links_XXX.json\n",
    "  python scraper/cleaning.py --input data/raw/raw_listings_XXX.csv\n"
  )
}

csv_file <- csv_files[length(csv_files)]
cat("Loading:", csv_file, "\n")

#  Read 
df <- read_csv(csv_file,
               locale         = locale(encoding = "UTF-8"),
               show_col_types = FALSE)

cat("Shape   :", nrow(df), "x", ncol(df), "\n")
cat("Columns :", paste(names(df), collapse = ", "), "\n\n")

# Numeric coercions
# These are the exact names cleaning.py outputs
num_cols <- c(
  "price", "GrossSquareMeters", "HallSquareMeters",
  "buildingAge", "numberOfBathrooms", "numberOfBalconies",
  "numberFloorsOfBuilding", "rentalIncome", "subscription",
  "adUpdateMonth", "adUpdateYear", "adActiveDays",
  "room_count", "price_per_m2",
  "BalconySquareMeters", "WCSquareMeters"
)
for (col in num_cols) {
  if (col %in% names(df))
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
}

# factor coercions 
cat_cols <- c(
  "listing_type", "district", "neighbourhood",
  "NumberOfRooms", "FloorLocation", "HeatingType", "KitchenType",
  "ItemStatus", "Elevator", "Parking", "InsideTheSite",
  "UsingStatus", "BuildStatus", "TitleStatus", "TitleType",
  "StructureType", "BalconyType", "Balcony",
  "CreditEligibility", "EligibilityForInvestment",
  "MortgageStatus", "Swap", "IsItVideoNavigable?",
  "PropertyType", "FromWhom", "SiteName", "EnergyRating"
)
for (col in cat_cols) {
  if (col %in% names(df))
    df[[col]] <- as.factor(df[[col]])
}

# helpers
yes_vals <- c("Var", "var", "Evet", "evet", "TRUE", "1", "true")

df <- df %>%
  mutate(
    Elevator_lgl      = as.character(Elevator)      %in% yes_vals,
    Parking_lgl       = as.character(Parking)       %in% yes_vals,
    InsideTheSite_lgl = as.character(InsideTheSite) %in% yes_vals,
    Furnished_lgl     = as.character(ItemStatus)    %in%
      c("Eşyalı", "esyali", "Evet", "evet")
  )

#price_per_m2 (recalculate if missing or all NA)
if (!"price_per_m2" %in% names(df) || all(is.na(df$price_per_m2))) {
  df <- df %>%
    mutate(price_per_m2 = ifelse(
      !is.na(GrossSquareMeters) & GrossSquareMeters > 0,
      price / GrossSquareMeters,
      NA_real_
    ))
}

# sale / rent 
df_satilik <- df %>% filter(listing_type == "satilik")
df_kiralik <- df %>% filter(listing_type == "kiralik")

cat("Total listings  :", nrow(df),         "\n")
cat("Sale listings   :", nrow(df_satilik), "\n")
cat("Rent listings   :", nrow(df_kiralik), "\n")

#  quick summary 
cat("\nPrice summary:\n")
print(summary(df$price))

cat("\nMissing values (top 15 columns with most NAs):\n")
mis <- sort(colSums(is.na(df)), decreasing = TRUE)
print(head(mis[mis > 0], 15))

# create output directories
for (d in c(
  here("data", "processed"),
  here("models"),
  here("visuals", "histograms"),
  here("visuals", "boxplots"),
  here("visuals", "heatmaps"),
  here("visuals", "regression_plots")
)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

#  Save .rds 
proc_dir <- here("data", "processed")
saveRDS(df,         file.path(proc_dir, "all_listings.rds"))
saveRDS(df_satilik, file.path(proc_dir, "satilik.rds"))
saveRDS(df_kiralik, file.path(proc_dir, "kiralik.rds"))

cat("\n✓  .rds files saved to data/processed/\n")
cat("Next: source('r-analysis/03_descriptive_statistics.R')\n")