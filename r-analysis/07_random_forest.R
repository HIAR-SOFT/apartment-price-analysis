# 07_random_forest.R
# Random Forest regression -- same feature set as 06.


library(dplyr)
library(caret)
library(randomForest)
library(ggplot2)
library(here)

set.seed(42)

df <- readRDS(here("data", "processed", "all_listings.rds"))

# build modelling dataframe
model_df <- df %>%
  filter(listing_type == "satilik") %>%
  transmute(
    price             = as.numeric(price),
    GrossSquareMeters = as.numeric(GrossSquareMeters),
    NetSquareMeters   = as.numeric(NetSquareMeters),
    rooms             = as.numeric(rooms),
    bathroom_count    = as.numeric(bathroom_count),
    building_age      = as.numeric(building_age),
    is_in_complex = case_when(
      is_in_complex %in% c(TRUE,  "TRUE",  "true",  "1") ~ TRUE,
      is_in_complex %in% c(FALSE, "FALSE", "false", "0") ~ FALSE,
      TRUE ~ FALSE
    ),
    heating_type       = as.character(heating_type),
    building_condition = as.character(building_condition),
    district           = as.character(district)
  ) %>%
  filter(!is.na(price), !is.na(GrossSquareMeters), !is.na(rooms),
         price > 0, GrossSquareMeters > 0) %>%
  mutate(
    log_price       = log(price),
    bathroom_count  = ifelse(is.na(bathroom_count),
                             median(bathroom_count, na.rm = TRUE), bathroom_count),
    building_age    = ifelse(is.na(building_age),
                             median(building_age,   na.rm = TRUE), building_age),
    NetSquareMeters = ifelse(is.na(NetSquareMeters), GrossSquareMeters, NetSquareMeters),
    heating_type       = ifelse(is.na(heating_type)       | heating_type       == "NA", "Unknown", heating_type),
    building_condition = ifelse(is.na(building_condition) | building_condition == "NA", "Unknown", building_condition),
    district           = ifelse(is.na(district)           | district           == "NA", "Unknown", district),
    # Collapse rare heating_type levels (< 5 rows) into "Other"
    heating_type = {
      ht   <- heating_type
      keep <- names(which(table(ht) >= 5))
      ifelse(ht %in% keep, ht, "Other")
    }
  ) %>%
  group_by(district) %>%
  filter(n() >= 5) %>%
  ungroup() %>%
  # Factorize on the FULL dataset BEFORE splitting
  mutate(
    heating_type       = factor(heating_type),
    building_condition = factor(building_condition),
    district           = factor(district)
  )

# Capture full level sets before any split
factor_cols   <- c("heating_type", "building_condition", "district")
factor_levels <- lapply(setNames(factor_cols, factor_cols),
                        function(col) levels(model_df[[col]]))

cat("Modelling dataset:", nrow(model_df), "rows\n")

if (nrow(model_df) < 20) {
  stop(paste(
    "Not enough rows for Random Forest (found", nrow(model_df), ").",
    "\nRun scraper to collect more listings."
  ))
}

RF_FEATURES <- c("GrossSquareMeters", "NetSquareMeters", "rooms",
                 "bathroom_count", "building_age", "is_in_complex",
                 "heating_type", "building_condition", "district")

# Train / test split 
train_idx <- createDataPartition(model_df$log_price, p = 0.8, list = FALSE)
train_df  <- model_df[ train_idx, ]
test_df   <- model_df[-train_idx, ]

# reapply the full level set to both splits
for (col in factor_cols) {
  train_df[[col]] <- factor(train_df[[col]], levels = factor_levels[[col]])
  test_df[[col]]  <- factor(test_df[[col]],  levels = factor_levels[[col]])
}

cat("Train:", nrow(train_df), "  Test:", nrow(test_df), "\n")

# train random forest
cat("\nTraining Random Forest (ntree=300) -- takes ~2-5 min ...\n")

rf_model <- randomForest(
  x          = train_df[, RF_FEATURES],
  y          = train_df$log_price,
  ntree      = 300,
  mtry       = max(1, floor(sqrt(length(RF_FEATURES)))),
  importance = TRUE,
  do.trace   = 100
)

print(rf_model)

# evaluate
test_df$pred_log   <- predict(rf_model, newdata = test_df[, RF_FEATURES])
test_df$pred_price <- exp(test_df$pred_log)

rmse <- sqrt(mean((test_df$price - test_df$pred_price)^2, na.rm = TRUE))
mae  <- mean(abs(test_df$price  - test_df$pred_price),    na.rm = TRUE)
r2   <- cor(test_df$price, test_df$pred_price, use = "complete.obs")^2

cat("\n RF MODEL EVALUATION \n")
cat(sprintf("RMSE : %s TL\n", format(round(rmse), big.mark = ",")))
cat(sprintf("MAE  : %s TL\n", format(round(mae),  big.mark = ",")))
cat(sprintf("R2   : %.4f\n",  r2))

# feature importance plot
plot_dir <- here("visuals", "regression_plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

imp <- as.data.frame(importance(rf_model)) %>%
  tibble::rownames_to_column("feature") %>%
  arrange(desc(`%IncMSE`))

p_fi <- ggplot(imp, aes(x = reorder(feature, `%IncMSE`), y = `%IncMSE`)) +
  geom_col(fill = "#FF5722", alpha = 0.85) +
  coord_flip() +
  labs(title    = "Random Forest -- Feature Importance",
       subtitle = "% Increase in MSE when feature is permuted",
       x = NULL, y = "% Increase in MSE") +
  theme_minimal(base_size = 13)

ggsave(file.path(plot_dir, "rf_feature_importance.png"),
       p_fi, width = 9, height = 6, dpi = 150)
cat("saved: visuals/regression_plots/rf_feature_importance.png\n")

# Save
models_dir <- here("models")
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(rf_model,                   file.path(models_dir, "random_forest_model.rds"))
saveRDS(list(features = RF_FEATURES, factor_levels = factor_levels), file.path(models_dir, "rf_features.rds"))
saveRDS(data.frame(model = "Random Forest", RMSE = rmse, MAE = mae, R2 = r2),
        file.path(models_dir, "rf_metrics.rds"))

cat("\n Nice! Random Forest model saved \n")
cat("Next: source('r-analysis/08_model_comparison.R')\n")