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

# ── TEST 1: Kitchen type vs price (ANOVA) ────────────────────
cat("── TEST 1: Kitchen Type vs Price ───────────────────────────────\n")

if (!"KitchenType" %in% names(df_s)) {
  cat("[SKIP] KitchenType column not found.\n\n")
} else {
  kitchen_counts <- df_s %>%
    filter(!is.na(KitchenType), as.character(KitchenType) != "NA") %>%
    count(KitchenType) %>%
    filter(n >= 10)          # lowered from 20 — more forgiving with small datasets
  
  if (nrow(kitchen_counts) < 2) {
    cat("[SKIP] Not enough kitchen type groups with n>=10.\n\n")
  } else {
    df_kitchen <- df_s %>%
      filter(as.character(KitchenType) %in% as.character(kitchen_counts$KitchenType))
    
    cat("Groups used:", paste(kitchen_counts$KitchenType, collapse = ", "), "\n")
    cat("Sample sizes:\n"); print(kitchen_counts)
    
    aov_k <- aov(price ~ KitchenType, data = df_kitchen)
    sum_k <- summary(aov_k)
    print(sum_k)
    
    p_k <- sum_k[[1]]$`Pr(>F)`[1]
    cat(sprintf("\nDecision: p = %.4f → %s H0 at α=0.05\n\n",
                p_k, ifelse(p_k < 0.05, "REJECT", "FAIL TO REJECT")))
    
    if (p_k < 0.05) {
      cat("Tukey HSD post-hoc:\n")
      print(TukeyHSD(aov_k))
    }
  }
}

# ── TEST 2: District vs price (ANOVA) ────────────────────────
cat("── TEST 2: District vs Price ───────────────────────────────────\n")

if (!"district" %in% names(df_s)) {
  cat("[SKIP] district column not found.\n\n")
} else {
  top_dist <- df_s %>%
    filter(!is.na(district), as.character(district) != "NA") %>%
    count(district) %>%
    slice_max(n, n = 10) %>%
    pull(district)
  
  df_dist <- df_s %>%
    filter(as.character(district) %in% as.character(top_dist))
  
  if (length(unique(as.character(df_dist$district))) < 2) {
    cat("[SKIP] Not enough distinct districts.\n\n")
  } else {
    aov_d <- aov(price ~ district, data = df_dist)
    sum_d <- summary(aov_d)
    print(sum_d)
    p_d   <- sum_d[[1]]$`Pr(>F)`[1]
    cat(sprintf("\nDecision: p = %.4f → %s H0 at α=0.05\n\n",
                p_d, ifelse(p_d < 0.05, "REJECT", "FAIL TO REJECT")))
  }
}

# ── TEST 3: Elevator vs price (Welch t-test) ─────────────────
cat("── TEST 3: Elevator vs No Elevator (t-test) ────────────────────\n")

if (!"Elevator_lgl" %in% names(df_s)) {
  cat("[SKIP] Elevator_lgl not found — run 01_data_loading.R first.\n\n")
} else {
  with_e    <- df_s %>% filter(isTRUE(Elevator_lgl)) %>% pull(price)
  without_e <- df_s %>% filter(!isTRUE(Elevator_lgl)) %>% pull(price)
  
  cat("With elevator   : n =", length(with_e))
  if (length(with_e) > 0)
    cat("  mean =", format(round(mean(with_e)), big.mark = ","), "TL")
  cat("\n")
  cat("Without elevator: n =", length(without_e))
  if (length(without_e) > 0)
    cat("  mean =", format(round(mean(without_e)), big.mark = ","), "TL")
  cat("\n\n")
  
  if (length(with_e) > 1 && length(without_e) > 1) {
    t_res <- t.test(with_e, without_e)
    print(t_res)
    cat(sprintf("\nDecision: p = %.4f → %s H0 at α=0.05\n\n",
                t_res$p.value,
                ifelse(t_res$p.value < 0.05, "REJECT", "FAIL TO REJECT")))
  } else {
    cat("[SKIP] Not enough observations in one or both groups.\n\n")
  }
}

# ── TEST 4: Normality of log prices (Shapiro-Wilk) ───────────
cat("── TEST 4: Log-Price Normality (Shapiro-Wilk) ──────────────────\n")

log_p <- log(df_s$price[!is.na(df_s$price) & df_s$price > 0])
if (length(log_p) < 3) {
  cat("[SKIP] Not enough price observations.\n\n")
} else {
  # Shapiro-Wilk requires n <= 5000
  samp <- sample(log_p, min(length(log_p), 5000))
  sw   <- shapiro.test(samp)
  print(sw)
  cat(sprintf("\nConclusion: Log prices %s normally distributed (p = %.4f)\n\n",
              ifelse(sw$p.value > 0.05, "ARE", "are NOT"), sw$p.value))
}

# ── TEST 5: InsideTheSite vs price (t-test) ──────────────────
cat("── TEST 5: Gated Complex vs Non-Gated (t-test) ─────────────────\n")

if (!"InsideTheSite_lgl" %in% names(df_s)) {
  cat("[SKIP] InsideTheSite_lgl not found — run 01_data_loading.R first.\n\n")
} else {
  in_site  <- df_s %>% filter(isTRUE(InsideTheSite_lgl))  %>% pull(price)
  out_site <- df_s %>% filter(!isTRUE(InsideTheSite_lgl)) %>% pull(price)
  
  cat("Inside site : n =", length(in_site))
  if (length(in_site) > 0)
    cat("  mean =", format(round(mean(in_site)), big.mark = ","), "TL")
  cat("\n")
  cat("Outside site: n =", length(out_site))
  if (length(out_site) > 0)
    cat("  mean =", format(round(mean(out_site)), big.mark = ","), "TL")
  cat("\n\n")
  
  if (length(in_site) > 1 && length(out_site) > 1) {
    t_site <- t.test(in_site, out_site)
    print(t_site)
    cat(sprintf("\nDecision: p = %.4f → %s H0 at α=0.05\n\n",
                t_site$p.value,
                ifelse(t_site$p.value < 0.05, "REJECT", "FAIL TO REJECT")))
  } else {
    cat("[SKIP] Not enough observations in one or both groups.\n\n")
  }
}

cat("Hypothesis testing complete.\n")
cat("Next: source('r-analysis/06_regression_model.R')\n")