# 06_regression_model.R


library(dplyr)
library(caret)
library(ggplot2)
library(scales)
library(here)

set.seed(42)

df <- readRDS(here("data", "processed", "all_listings.rds"))

# Build modelling dataframe 
model_df <- df %>%
  filter(listing_type == "satilik") %>%
  transmute(
    price             = suppressWarnings(as.numeric(price)),
    GrossSquareMeters = suppressWarnings(as.numeric(GrossSquareMeters)),
    NetSquareMeters   = suppressWarnings(as.numeric(NetSquareMeters)),
    rooms             = suppressWarnings(as.numeric(rooms)),
    bathroom_count    = suppressWarnings(as.numeric(bathroom_count)),
    building_age      = suppressWarnings(as.numeric(building_age)),
    # is_in_complex replaces Elevator / Parking / InsideTheSite
    is_in_complex = case_when(
      is_in_complex %in% c(TRUE,  "TRUE",  "true",  "1") ~ TRUE,
      is_in_complex %in% c(FALSE, "FALSE", "false", "0") ~ FALSE,
      TRUE ~ FALSE
    ),
    # Categorical features as character first for safe cleaning
    heating_type       = if ("heating_type"       %in% names(df)) as.character(heating_type)       else "Unknown",
    building_condition = if ("building_condition" %in% names(df)) as.character(building_condition) else "Unknown",
    furnished          = if ("furnished"          %in% names(df)) as.character(furnished)          else "Unknown",
    district           = if ("district"           %in% names(df)) as.character(district)           else "Unknown"
  ) %>%
  filter(!is.na(price), !is.na(GrossSquareMeters),
         price > 0, GrossSquareMeters > 0) %>%
  mutate(
    log_price       = log(price),
    rooms           = ifelse(is.na(rooms), 2L, rooms),
    bathroom_count  = ifelse(is.na(bathroom_count),
                             median(bathroom_count, na.rm = TRUE), bathroom_count),
    building_age    = ifelse(is.na(building_age),
                             median(building_age,   na.rm = TRUE), building_age),
    NetSquareMeters = ifelse(is.na(NetSquareMeters), GrossSquareMeters, NetSquareMeters),
    # Clean NAs /empty strings in categoricals
    heating_type       = ifelse(is.na(heating_type)       | heating_type       %in% c("NA", ""), "Unknown", heating_type),
    building_condition = ifelse(is.na(building_condition) | building_condition %in% c("NA", ""), "Unknown", building_condition),
    furnished        = ifelse(is.na(furnished)          | furnished          %in% c("NA", ""), "Unknown", furnished),
    district           = ifelse(is.na(district)           | district           %in% c("NA", ""), "Unknown", district),
    # Collapse rare heating_type levels (< 5 rows) into "Other" so
    # every factor level is guaranteed to exist in both train and test.
    heating_type = {
      ht   <- heating_type
      keep <- names(which(table(ht) >= 5))
      ifelse(ht %in% keep, ht, "Other")
    }
  ) %>%
  group_by(district) %>%
  filter(n() >= 3) %>%
  ungroup() %>%
  # Factorize on the FULL dataset BEFORE splitting so levels are in train and test
  
  mutate(
    heating_type       = factor(heating_type),
    building_condition = factor(building_condition),
    furnished          = factor(furnished),
    district           = factor(district)
  )

# Capture the full level sets from model_df
factor_cols   <- c("heating_type", "building_condition", "furnished", "district")
factor_levels <- lapply(setNames(factor_cols, factor_cols),
                        function(col) levels(model_df[[col]]))

cat("Modelling dataset:", nrow(model_df), "rows,",
    length(unique(model_df$district)), "districts\n")

if (nrow(model_df) < 20) {
  stop(paste(
    "Not enough rows for regression (found", nrow(model_df), ").",
    "\nRun scraper to collect more listings."
  ))
}

# feature selection
base_features  <- c("GrossSquareMeters", "NetSquareMeters", "rooms",
                    "bathroom_count", "building_age", "is_in_complex")

extra_features <- character(0)
for (feat in factor_cols) {
  if (feat %in% names(model_df) && nlevels(model_df[[feat]]) >= 2)
    extra_features <- c(extra_features, feat)
}

all_features <- c(base_features, extra_features)
lm_formula   <- as.formula(
  paste("log_price ~", paste(all_features, collapse = " + "))
)
cat("Formula:", deparse(lm_formula), "\n\n")

# Train/test split 
train_idx <- createDataPartition(model_df$log_price, p = 0.8, list = FALSE)
train_df  <- model_df[ train_idx, ]
test_df   <- model_df[-train_idx, ]

# Reapply the full level set to both splits so no split is missing a level that the other split (and the model) knows about
for (col in factor_cols) {
  train_df[[col]] <- factor(train_df[[col]], levels = factor_levels[[col]])
  test_df[[col]]  <- factor(test_df[[col]],  levels = factor_levels[[col]])
}

cat("Train:", nrow(train_df), "  Test:", nrow(test_df), "\n")

# fit linear regression
lm_model <- lm(lm_formula, data = train_df)
cat("\n=== LINEAR REGRESSION SUMMARY ===\n")
print(summary(lm_model))

# evaluate on test set
test_df$pred_log   <- predict(lm_model, newdata = test_df)
test_df$pred_price <- exp(test_df$pred_log)

rmse <- sqrt(mean((test_df$price - test_df$pred_price)^2, na.rm = TRUE))
mae  <- mean(abs(test_df$price  - test_df$pred_price),    na.rm = TRUE)
r2   <- cor(test_df$price, test_df$pred_price, use = "complete.obs")^2

cat("\n=== MODEL EVALUATION ===\n")
cat(sprintf("RMSE : %s TL\n", format(round(rmse), big.mark = ",")))
cat(sprintf("MAE  : %s TL\n", format(round(mae),  big.mark = ",")))
cat(sprintf("R2   : %.4f\n",  r2))

# Plots
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
  labs(title = "Actual vs Predicted - Linear Regression",
       x = "Actual (M TL)", y = "Predicted (M TL)") +
  theme_minimal(base_size = 13)
ggsave(file.path(plot_dir, "lm_actual_vs_predicted.png"), p_avp, width = 8, height = 6, dpi = 150)
cat("Regression plots saved.\n")

# Save model
models_dir <- here("models")
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(lm_model, file.path(models_dir, "linear_regression_model.rds"))
saveRDS(data.frame(model = "Linear Regression", RMSE = rmse, MAE = mae, R2 = r2),
        file.path(models_dir, "lm_metrics.rds"))

cat("\n Linear regression model saved.\n")
cat("Next: source('r-analysis/07_random_forest.R')\n")