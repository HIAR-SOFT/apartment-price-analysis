# ============================================================
# 07_random_forest.R
# Trains a Random Forest regression model on Istanbul
# apartment sale prices. Saves model + feature importance.
# ============================================================

library(dplyr)
library(caret)
library(randomForest)
library(ggplot2)

set.seed(42)

df <- readRDS("data/processed/all_listings.rds")

# ── 1. Prepare dataset ────────────────────────────────────────
model_df <- df %>%
  filter(listing_type == "satilik") %>%
  select(
    price, net_area_m2, room_count, bathroom_count, floor_number,
    building_age, elevator, parking, within_site, furnished,
    district, heating_type
  ) %>%
  filter(if_all(c(price, net_area_m2, room_count), ~ !is.na(.))) %>%
  mutate(log_price = log(price)) %>%
  mutate(
    bathroom_count = ifelse(is.na(bathroom_count), median(bathroom_count, na.rm = TRUE), bathroom_count),
    floor_number   = ifelse(is.na(floor_number),   median(floor_number,   na.rm = TRUE), floor_number),
    building_age   = ifelse(is.na(building_age),   median(building_age,   na.rm = TRUE), building_age),
    elevator    = as.factor(ifelse(is.na(elevator),   FALSE, elevator)),
    parking     = as.factor(ifelse(is.na(parking),    FALSE, parking)),
    within_site = as.factor(ifelse(is.na(within_site), FALSE, within_site)),
    furnished   = as.factor(ifelse(is.na(furnished),   FALSE, furnished)),
    district    = as.factor(ifelse(is.na(district), "Unknown", as.character(district))),
    heating_type = as.factor(ifelse(is.na(heating_type), "Unknown", as.character(heating_type)))
  ) %>%
  # Remove levels with very few listings (causes issues in train/test split)
  group_by(district) %>%
  filter(n() >= 10) %>%
  ungroup()

cat("Modelling dataset:", nrow(model_df), "rows\n")

# ── 2. Train/test split ───────────────────────────────────────
train_idx <- createDataPartition(model_df$log_price, p = 0.8, list = FALSE)
train_df  <- model_df[train_idx, ]
test_df   <- model_df[-train_idx, ]

cat("Train:", nrow(train_df), "  Test:", nrow(test_df), "\n")

# ── 3. Train Random Forest ────────────────────────────────────
cat("\nTraining Random Forest (ntree=300)… this may take a few minutes.\n")

rf_features <- c("net_area_m2", "room_count", "bathroom_count", "floor_number",
                  "building_age", "elevator", "parking", "within_site",
                  "furnished", "district", "heating_type")

rf_model <- randomForest(
  x      = train_df[, rf_features],
  y      = train_df$log_price,
  ntree  = 300,
  mtry   = floor(sqrt(length(rf_features))),
  importance = TRUE,
  do.trace   = 50
)

print(rf_model)

# ── 4. Evaluate on test set ───────────────────────────────────
test_df$predicted_log   <- predict(rf_model, newdata = test_df[, rf_features])
test_df$predicted_price <- exp(test_df$predicted_log)

rmse <- sqrt(mean((test_df$price - test_df$predicted_price)^2, na.rm = TRUE))
mae  <- mean(abs(test_df$price - test_df$predicted_price), na.rm = TRUE)
r2   <- cor(test_df$price, test_df$predicted_price, use = "complete.obs")^2

cat("\n=== RF MODEL EVALUATION (test set) ===\n")
cat(sprintf("RMSE : %s TL\n", format(round(rmse), big.mark = ",")))
cat(sprintf("MAE  : %s TL\n", format(round(mae),  big.mark = ",")))
cat(sprintf("R²   : %.4f\n",  r2))

# ── 5. Feature importance plot ────────────────────────────────
imp <- as.data.frame(importance(rf_model)) %>%
  tibble::rownames_to_column("feature") %>%
  arrange(desc(`%IncMSE`))

fi_plot <- ggplot(imp, aes(x = reorder(feature, `%IncMSE`), y = `%IncMSE`)) +
  geom_col(fill = "#FF5722", alpha = 0.85) +
  coord_flip() +
  labs(
    title = "Random Forest — Feature Importance",
    subtitle = "% Increase in MSE when feature is permuted",
    x = NULL, y = "% Increase in MSE"
  ) +
  theme_minimal(base_size = 13)

ggsave("visuals/regression_plots/rf_feature_importance.png", fi_plot, width = 9, height = 6, dpi = 150)
cat("Saved: visuals/regression_plots/rf_feature_importance.png\n")

# ── 6. Save model + metrics ───────────────────────────────────
saveRDS(rf_model, "models/random_forest_model.rds")

metrics_rf <- data.frame(model = "Random Forest", RMSE = rmse, MAE = mae, R2 = r2)
saveRDS(metrics_rf, "models/rf_metrics.rds")

cat("\n✓ Random Forest model saved to models/random_forest_model.rds\n")