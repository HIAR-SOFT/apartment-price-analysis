# ============================================================
# 06_regression_model.R
# Builds and evaluates a multiple linear regression model.
# Saves the trained model to models/linear_regression_model.rds
# ============================================================

library(dplyr)
library(caret)
library(ggplot2)
library(scales)

set.seed(42)

df <- readRDS("data/processed/all_listings.rds")

# ── 1. Prepare modelling dataset ─────────────────────────────
model_df <- df %>%
  filter(listing_type == "satilik") %>%
  select(
    price, net_area_m2, room_count, bathroom_count, floor_number,
    building_age, elevator, parking, within_site, furnished,
    district, heating_type, kitchen_type
  ) %>%
  # Drop rows with NA in any modelling column
  filter(if_all(c(price, net_area_m2, room_count), ~ !is.na(.))) %>%
  # Log-transform price (improves linearity)
  mutate(log_price = log(price)) %>%
  # Fill remaining NAs with medians/modes
  mutate(
    bathroom_count = ifelse(is.na(bathroom_count), median(bathroom_count, na.rm = TRUE), bathroom_count),
    floor_number   = ifelse(is.na(floor_number),   median(floor_number,   na.rm = TRUE), floor_number),
    building_age   = ifelse(is.na(building_age),   median(building_age,   na.rm = TRUE), building_age),
    elevator       = ifelse(is.na(elevator),   FALSE, elevator),
    parking        = ifelse(is.na(parking),    FALSE, parking),
    within_site    = ifelse(is.na(within_site), FALSE, within_site),
    furnished      = ifelse(is.na(furnished),   FALSE, furnished)
  )

cat("Modelling dataset:", nrow(model_df), "rows\n")

# ── 2. Train/test split ───────────────────────────────────────
train_idx <- createDataPartition(model_df$log_price, p = 0.8, list = FALSE)
train_df  <- model_df[train_idx, ]
test_df   <- model_df[-train_idx, ]

cat("Train:", nrow(train_df), "  Test:", nrow(test_df), "\n")

# ── 3. Fit linear regression ──────────────────────────────────
# Using log_price as response to handle right skew
lm_formula <- log_price ~ net_area_m2 + room_count + bathroom_count +
  floor_number + building_age + elevator + parking +
  within_site + furnished + district + heating_type + kitchen_type

lm_model <- lm(lm_formula, data = train_df)

cat("\n=== LINEAR REGRESSION SUMMARY ===\n")
print(summary(lm_model))

# ── 4. Evaluate on test set ───────────────────────────────────
test_df$predicted_log <- predict(lm_model, newdata = test_df)
test_df$predicted_price <- exp(test_df$predicted_log)

rmse <- sqrt(mean((test_df$price - test_df$predicted_price)^2, na.rm = TRUE))
mae  <- mean(abs(test_df$price - test_df$predicted_price), na.rm = TRUE)
r2   <- cor(test_df$price, test_df$predicted_price, use = "complete.obs")^2

cat("\n=== MODEL EVALUATION (on test set) ===\n")
cat(sprintf("RMSE : %s TL\n", format(round(rmse), big.mark = ",")))
cat(sprintf("MAE  : %s TL\n", format(round(mae),  big.mark = ",")))
cat(sprintf("R²   : %.4f\n",  r2))

# ── 5. Residual plot ──────────────────────────────────────────
res_plot <- ggplot(data.frame(
    fitted   = test_df$predicted_price / 1e6,
    residual = (test_df$price - test_df$predicted_price) / 1e6
  ), aes(x = fitted, y = residual)) +
  geom_point(alpha = 0.4, size = 0.8, color = "#2196F3") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  scale_x_continuous(labels = label_number(suffix = "M")) +
  scale_y_continuous(labels = label_number(suffix = "M")) +
  labs(
    title = "Linear Regression: Residuals vs Fitted",
    x = "Fitted Price (million TL)", y = "Residual (million TL)"
  ) +
  theme_minimal(base_size = 13)

ggsave("visuals/regression_plots/lm_residuals.png", res_plot, width = 9, height = 6, dpi = 150)

# Actual vs Predicted
avp_plot <- ggplot(test_df, aes(x = price / 1e6, y = predicted_price / 1e6)) +
  geom_point(alpha = 0.3, size = 0.7, color = "#2196F3") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  scale_x_continuous(labels = label_number(suffix = "M")) +
  scale_y_continuous(labels = label_number(suffix = "M")) +
  labs(
    title = "Actual vs Predicted Sale Prices (Linear Regression)",
    x = "Actual (million TL)", y = "Predicted (million TL)"
  ) +
  theme_minimal(base_size = 13)

ggsave("visuals/regression_plots/lm_actual_vs_predicted.png", avp_plot, width = 8, height = 6, dpi = 150)

# ── 6. Save model and metrics ─────────────────────────────────
saveRDS(lm_model, "models/linear_regression_model.rds")

metrics_lm <- data.frame(model = "Linear Regression", RMSE = rmse, MAE = mae, R2 = r2)
saveRDS(metrics_lm, "models/lm_metrics.rds")

cat("\n✓ Linear regression model saved to models/linear_regression_model.rds\n")