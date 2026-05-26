# ============================================================
# 06_regression_model.R
# ============================================================

library(dplyr)
library(caret)
library(ggplot2)
library(scales)
library(here)

set.seed(42)

df <- readRDS(here("data", "processed", "all_listings.rds"))

# ── 1. Build modelling dataframe ──────────────────────────────
model_df <- df %>%
  filter(listing_type == "satilik") %>%
  transmute(
    price             = suppressWarnings(as.numeric(price)),
    GrossSquareMeters = suppressWarnings(as.numeric(GrossSquareMeters)),
    room_count        = suppressWarnings(as.numeric(room_count)),
    numberOfBathrooms = suppressWarnings(as.numeric(numberOfBathrooms)),
    buildingAge       = suppressWarnings(as.numeric(buildingAge)),
    Elevator          = isTRUE(Elevator_lgl),
    Parking           = isTRUE(Parking_lgl),
    InsideTheSite     = isTRUE(InsideTheSite_lgl),
    # Use columns only if they exist
    KitchenType = if ("KitchenType" %in% names(df)) as.character(KitchenType) else "Unknown",
    HeatingType = if ("HeatingType" %in% names(df)) as.character(HeatingType) else "Unknown",
    district    = if ("district"    %in% names(df)) as.character(district)    else "Unknown"
  ) %>%
  filter(!is.na(price), !is.na(GrossSquareMeters),
         price > 0, GrossSquareMeters > 0) %>%
  mutate(
    log_price         = log(price),
    room_count        = ifelse(is.na(room_count),        2,    room_count),
    numberOfBathrooms = ifelse(is.na(numberOfBathrooms),
                               median(numberOfBathrooms, na.rm = TRUE), numberOfBathrooms),
    buildingAge       = ifelse(is.na(buildingAge),
                               median(buildingAge,       na.rm = TRUE), buildingAge),
    KitchenType = ifelse(is.na(KitchenType) | KitchenType %in% c("NA",""), "Bilinmiyor", KitchenType),
    HeatingType = ifelse(is.na(HeatingType) | HeatingType %in% c("NA",""), "Bilinmiyor", HeatingType),
    district    = ifelse(is.na(district)    | district    %in% c("NA",""), "Bilinmiyor", district),
    KitchenType = as.factor(KitchenType),
    HeatingType = as.factor(HeatingType),
    district    = as.factor(district)
  ) %>%
  group_by(district) %>%
  filter(n() >= 3) %>%       # lowered threshold for small datasets
  ungroup()

cat("Modelling dataset:", nrow(model_df), "rows,",
    length(unique(model_df$district)), "districts\n")

if (nrow(model_df) < 20) {
  stop(paste(
    "Not enough rows for regression (found", nrow(model_df), ").",
    "\nRun scraper to collect more listings."
  ))
}

# ── 2. Decide which features to use based on data availability ─
base_features <- c("GrossSquareMeters", "room_count",
                   "numberOfBathrooms", "buildingAge",
                   "Elevator", "Parking", "InsideTheSite")

# Add categorical features only if they have 2+ levels
extra_features <- character(0)
for (feat in c("HeatingType", "KitchenType", "district")) {
  lvls <- nlevels(model_df[[feat]])
  if (lvls >= 2) extra_features <- c(extra_features, feat)
}

all_features  <- c(base_features, extra_features)
lm_formula    <- as.formula(
  paste("log_price ~", paste(all_features, collapse = " + "))
)
cat("Formula:", deparse(lm_formula), "\n\n")

# ── 3. Train / test split ─────────────────────────────────────
train_idx <- createDataPartition(model_df$log_price, p = 0.8, list = FALSE)
train_df  <- model_df[ train_idx, ]
test_df   <- model_df[-train_idx, ]
cat("Train:", nrow(train_df), "  Test:", nrow(test_df), "\n")

# ── 4. Fit linear regression ──────────────────────────────────
lm_model <- lm(lm_formula, data = train_df)
cat("\n=== LINEAR REGRESSION SUMMARY ===\n")
print(summary(lm_model))

# ── 5. Evaluate on test set ───────────────────────────────────
test_df$pred_log   <- predict(lm_model, newdata = test_df)
test_df$pred_price <- exp(test_df$pred_log)

rmse <- sqrt(mean((test_df$price - test_df$pred_price)^2, na.rm = TRUE))
mae  <- mean(abs(test_df$price  - test_df$pred_price),    na.rm = TRUE)
r2   <- cor(test_df$price, test_df$pred_price, use = "complete.obs")^2

cat("\n=== MODEL EVALUATION ===\n")
cat(sprintf("RMSE : %s TL\n", format(round(rmse), big.mark = ",")))
cat(sprintf("MAE  : %s TL\n", format(round(mae),  big.mark = ",")))
cat(sprintf("R²   : %.4f\n",  r2))

# ── 6. Plots ──────────────────────────────────────────────────
plot_dir <- here("visuals", "regression_plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

res_df <- data.frame(
  fitted   = test_df$pred_price / 1e6,
  residual = (test_df$price - test_df$pred_price) / 1e6
)
p_res <- ggplot(res_df, aes(x = fitted, y = residual)) +
  geom_point(alpha = 0.4, size = 0.8, color = "#2196F3") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  scale_x_continuous(labels = label_number(suffix = "M")) +
  scale_y_continuous(labels = label_number(suffix = "M")) +
  labs(title = "Linear Regression: Residuals vs Fitted",
       x = "Fitted (M TL)", y = "Residual (M TL)") +
  theme_minimal(base_size = 13)
ggsave(file.path(plot_dir, "lm_residuals.png"), p_res, width = 9, height = 6, dpi = 150)

p_avp <- ggplot(test_df, aes(x = price / 1e6, y = pred_price / 1e6)) +
  geom_point(alpha = 0.3, size = 0.7, color = "#2196F3") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  scale_x_continuous(labels = label_number(suffix = "M")) +
  scale_y_continuous(labels = label_number(suffix = "M")) +
  labs(title = "Actual vs Predicted — Linear Regression",
       x = "Actual (M TL)", y = "Predicted (M TL)") +
  theme_minimal(base_size = 13)
ggsave(file.path(plot_dir, "lm_actual_vs_predicted.png"), p_avp, width = 8, height = 6, dpi = 150)
cat("Regression plots saved.\n")

# ── 7. Save model ─────────────────────────────────────────────
models_dir <- here("models")
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(lm_model, file.path(models_dir, "linear_regression_model.rds"))
saveRDS(data.frame(model = "Linear Regression", RMSE = rmse, MAE = mae, R2 = r2),
        file.path(models_dir, "lm_metrics.rds"))

cat("\n✓ Linear regression model saved.\n")
cat("Next: source('r-analysis/07_random_forest.R')\n")