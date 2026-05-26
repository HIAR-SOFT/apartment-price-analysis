# ============================================================
# 05_hypothesis_testing.R
# Performs t-tests and ANOVA to test:
#   H1: Kitchen type affects apartment prices
#   H2: Mean prices differ across districts
# ============================================================

library(dplyr)
library(ggplot2)
library(scales)

df <- readRDS("data/processed/all_listings.rds")

# Use only sale listings with valid prices
df_s <- df %>% filter(listing_type == "satilik", !is.na(price))

cat("=================================================================\n")
cat("HYPOTHESIS TESTING — Istanbul Apartment Prices\n")
cat("=================================================================\n\n")

# ── TEST 1: Does kitchen type affect price? (ANOVA) ───────────
cat("── TEST 1: Kitchen Type vs Price ───────────────────────────────\n")
cat("H0: Mean price is equal across all kitchen types.\n")
cat("H1: At least one kitchen type has a different mean price.\n\n")

kitchen_counts <- df_s %>%
  count(kitchen_type) %>%
  filter(!is.na(kitchen_type), n >= 30)  # only types with enough data

df_kitchen <- df_s %>%
  filter(kitchen_type %in% kitchen_counts$kitchen_type)

cat("Kitchen types used:", paste(kitchen_counts$kitchen_type, collapse = ", "), "\n")
cat("Sample sizes:\n")
print(kitchen_counts)

aov_kitchen <- aov(price ~ kitchen_type, data = df_kitchen)
summary_kitchen <- summary(aov_kitchen)
print(summary_kitchen)

p_val_kitchen <- summary_kitchen[[1]]$`Pr(>F)`[1]
cat(sprintf(
  "\nDecision: p = %.4f → %s H0 at α=0.05\n\n",
  p_val_kitchen,
  ifelse(p_val_kitchen < 0.05, "REJECT", "FAIL TO REJECT")
))

# Post-hoc Tukey test if H0 rejected
if (p_val_kitchen < 0.05) {
  cat("Tukey HSD post-hoc test:\n")
  print(TukeyHSD(aov_kitchen))
}

# ── TEST 2: Do prices differ across districts? (ANOVA) ────────
cat("── TEST 2: District vs Price ───────────────────────────────────\n")
cat("H0: Mean apartment prices are equal across all districts.\n")
cat("H1: At least one district has a significantly different mean.\n\n")

top_districts <- df_s %>%
  count(district) %>%
  top_n(10, n) %>%
  pull(district)

df_district <- df_s %>%
  filter(district %in% top_districts, !is.na(district))

aov_district <- aov(price ~ district, data = df_district)
summary_district <- summary(aov_district)
print(summary_district)

p_val_district <- summary_district[[1]]$`Pr(>F)`[1]
cat(sprintf(
  "\nDecision: p = %.4f → %s H0 at α=0.05\n\n",
  p_val_district,
  ifelse(p_val_district < 0.05, "REJECT", "FAIL TO REJECT")
))

# ── TEST 3: Do sale prices differ by elevator presence? (t-test) ─
cat("── TEST 3: Elevator vs No Elevator (t-test) ────────────────────\n")
cat("H0: Mean sale price is equal for apartments with and without elevator.\n")
cat("H1: Mean sale price differs.\n\n")

with_elev    <- df_s %>% filter(elevator == TRUE, !is.na(price)) %>% pull(price)
without_elev <- df_s %>% filter(elevator == FALSE, !is.na(price)) %>% pull(price)

cat("With elevator: n =", length(with_elev),
    "  mean =", format(mean(with_elev), big.mark = ","), "TL\n")
cat("Without elevator: n =", length(without_elev),
    "  mean =", format(mean(without_elev), big.mark = ","), "TL\n\n")

t_result <- t.test(with_elev, without_elev)
print(t_result)

p_val_t <- t_result$p.value
cat(sprintf(
  "\nDecision: p = %.4f → %s H0 at α=0.05\n",
  p_val_t,
  ifelse(p_val_t < 0.05, "REJECT", "FAIL TO REJECT")
))

# ── TEST 4: Sale vs Rental price normality (Shapiro-Wilk) ─────
cat("\n── TEST 4: Price Normality (Shapiro-Wilk on log prices) ────────\n")
log_prices <- log(df_s$price[!is.na(df_s$price)])
# Shapiro needs ≤5000 obs
sample_prices <- sample(log_prices, min(length(log_prices), 5000))
sw <- shapiro.test(sample_prices)
print(sw)

cat(sprintf(
  "\nConclusion: Log prices %s normally distributed (p = %.4f)\n",
  ifelse(sw$p.value > 0.05, "ARE", "are NOT"),
  sw$p.value
))

cat("\n✓ Hypothesis testing complete.\n")