# ============================================================
# 05_hypothesis_testing.R
# ============================================================

library(dplyr)
library(ggplot2)
library(scales)
library(here)

df   <- readRDS(here("data", "processed", "all_listings.rds"))
df_s <- df %>% filter(listing_type == "satilik", !is.na(price))

cat("=================================================================\n")
cat("HYPOTHESIS TESTING — Istanbul Apartment Prices\n")
cat("=================================================================\n\n")
cat("Sale listings available:", nrow(df_s), "\n\n")

# ── TEST 1: Heating type vs price (ANOVA) ────────────────────
# Replaces old KitchenType test — heating_type is the equivalent
# categorical building-service variable in the new dataset.
cat("── TEST 1: Heating Type vs Price (ANOVA) ───────────────────────\n")

if (!"heating_type" %in% names(df_s)) {
  cat("[SKIP] heating_type column not found.\n\n")
} else {
  heating_counts <- df_s %>%
    filter(!is.na(heating_type), heating_type != "NA") %>%
    count(heating_type) %>%
    filter(n >= 10)
  
  if (nrow(heating_counts) < 2) {
    cat("[SKIP] Not enough heating type groups with n >= 10.\n\n")
  } else {
    df_heating <- df_s %>%
      filter(heating_type %in% heating_counts$heating_type) %>%
      mutate(heating_type = factor(heating_type))
    
    cat("Groups used:", paste(heating_counts$heating_type, collapse = ", "), "\n")
    cat("Sample sizes:\n"); print(heating_counts)
    
    aov_h <- aov(price ~ heating_type, data = df_heating)
    sum_h <- summary(aov_h)
    print(sum_h)
    
    p_h <- sum_h[[1]]$`Pr(>F)`[1]
    cat(sprintf("\nDecision: p = %.4f → %s H0 at α=0.05\n\n",
                p_h, ifelse(p_h < 0.05, "REJECT", "FAIL TO REJECT")))
    
    if (p_h < 0.05) {
      cat("Tukey HSD post-hoc:\n")
      print(TukeyHSD(aov_h))
    }
  }
}

# ── TEST 2: District vs price (ANOVA) ────────────────────────
cat("── TEST 2: District vs Price (ANOVA) ───────────────────────────\n")

if (!"district" %in% names(df_s)) {
  cat("[SKIP] district column not found.\n\n")
} else {
  top_dist <- df_s %>%
    filter(!is.na(district), district != "NA") %>%
    count(district) %>%
    slice_max(n, n = 10) %>%
    pull(district)
  
  df_dist <- df_s %>%
    filter(district %in% top_dist) %>%
    mutate(district = factor(district))
  
  if (length(unique(df_dist$district)) < 2) {
    cat("[SKIP] Not enough distinct districts.\n\n")
  } else {
    aov_d <- aov(price ~ district, data = df_dist)
    sum_d <- summary(aov_d)
    print(sum_d)
    p_d <- sum_d[[1]]$`Pr(>F)`[1]
    cat(sprintf("\nDecision: p = %.4f → %s H0 at α=0.05\n\n",
                p_d, ifelse(p_d < 0.05, "REJECT", "FAIL TO REJECT")))
  }
}

# TEST 3: is_in_complex vs price (Welch t-test)
cat("── TEST 3: In Gated Complex vs Not (Welch t-test) ──────────────\n")

if (!"is_in_complex" %in% names(df_s)) {
  cat("[SKIP] is_in_complex not found.\n\n")
} else {
  # Coerce to logical, python writes TRUE/FALSE but read_csv may import as character or logical depending on version
  df_s <- df_s %>%
    mutate(is_in_complex_lgl = case_when(
      is_in_complex %in% c(TRUE,  "TRUE",  "true",  "1") ~ TRUE,
      is_in_complex %in% c(FALSE, "FALSE", "false", "0") ~ FALSE,
      TRUE ~ NA
    ))
  
  in_complex  <- df_s %>% filter(isTRUE(is_in_complex_lgl)) %>% pull(price)
  out_complex <- df_s %>% filter(isFALSE(is_in_complex_lgl)) %>% pull(price)
  
  cat("In complex  : n =", length(in_complex))
  if (length(in_complex) > 0)
    cat("  mean =", format(round(mean(in_complex)), big.mark = ","), "TL")
  cat("\n")
  cat("Not in complex: n =", length(out_complex))
  if (length(out_complex) > 0)
    cat("  mean =", format(round(mean(out_complex)), big.mark = ","), "TL")
  cat("\n\n")
  
  if (length(in_complex) > 1 && length(out_complex) > 1) {
    t_res <- t.test(in_complex, out_complex)
    print(t_res)
    cat(sprintf("\nDecision: p = %.4f → %s H0 at α=0.05\n\n",
                t_res$p.value,
                ifelse(t_res$p.value < 0.05, "REJECT", "FAIL TO REJECT")))
  } else {
    cat("[SKIP] Not enough observations in one or both groups.\n\n")
  }
}

# TEST 4: Normality of log prices (Shapiro-Wilk) 
cat(" TEST 4: Log-Price Normality \n")

log_p <- log(df_s$price[!is.na(df_s$price) & df_s$price > 0])
if (length(log_p) < 3) {
  cat("[SKIP] Not enough price observations.\n\n")
} else {
  samp <- sample(log_p, min(length(log_p), 5000))
  sw   <- shapiro.test(samp)
  print(sw)
  cat(sprintf("\nConclusion: Log prices %s normally distributed (p = %.4f)\n\n",
              ifelse(sw$p.value > 0.05, "ARE", "are NOT"), sw$p.value))
}

# TEST 5: Building condition vs price ─
# Replaces old InsideTheSite test — building_condition (New vs
# Second-hand) is a clean binary variable in the new dataset.
cat("── TEST 5: New vs Second-hand Buildings (Welch t-test) ─────────\n")

if (!"building_condition" %in% names(df_s)) {
  cat("[SKIP] building_condition not found.\n\n")
} else {
  new_bldg  <- df_s %>% filter(building_condition == "New")         %>% pull(price)
  used_bldg <- df_s %>% filter(building_condition == "Second-hand") %>% pull(price)
  
  cat("New buildings       : n =", length(new_bldg))
  if (length(new_bldg) > 0)
    cat("  mean =", format(round(mean(new_bldg)), big.mark = ","), "TL")
  cat("\n")
  cat("Second-hand buildings: n =", length(used_bldg))
  if (length(used_bldg) > 0)
    cat("  mean =", format(round(mean(used_bldg)), big.mark = ","), "TL")
  cat("\n\n")
  
  if (length(new_bldg) > 1 && length(used_bldg) > 1) {
    t_cond <- t.test(new_bldg, used_bldg)
    print(t_cond)
    cat(sprintf("\nDecision: p = %.4f → %s H0 at α=0.05\n\n",
                t_cond$p.value,
                ifelse(t_cond$p.value < 0.05, "REJECT", "FAIL TO REJECT")))
  } else {
    cat("[SKIP] Not enough observations in one or both groups.\n\n")
  }
}

# ── TEST 6: Furnished vs price (Welch t-test) — new test ──────
cat("── TEST 6: Furnished vs Unfurnished (Welch t-test) ─────────────\n")

if (!"furnished" %in% names(df_s)) {
  cat("[SKIP] furnished column not found.\n\n")
} else {
  furn   <- df_s %>% filter(furnished == "Furnished")   %>% pull(price)
  unfurn <- df_s %>% filter(furnished == "Unfurnished") %>% pull(price)
  
  cat("Furnished  : n =", length(furn))
  if (length(furn) > 0)
    cat("  mean =", format(round(mean(furn)), big.mark = ","), "TL")
  cat("\n")
  cat("Unfurnished: n =", length(unfurn))
  if (length(unfurn) > 0)
    cat("  mean =", format(round(mean(unfurn)), big.mark = ","), "TL")
  cat("\n\n")
  
  if (length(furn) > 1 && length(unfurn) > 1) {
    t_furn <- t.test(furn, unfurn)
    print(t_furn)
    cat(sprintf("\nDecision: p = %.4f → %s H0 at α=0.05\n\n",
                t_furn$p.value,
                ifelse(t_furn$p.value < 0.05, "REJECT", "FAIL TO REJECT")))
  } else {
    cat("[SKIP] Not enough observations in one or both groups.\n\n")
  }
}

cat("Hypothesis testing complete.\n")
cat("Next: source('r-analysis/06_regression_model.R')\n")