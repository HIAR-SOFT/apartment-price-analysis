# ============================================================
# 07_random_forest.R
# Random Forest regression — same feature set as 06 but
# using actual column names from cleaning.py output.
# ============================================================

library(dplyr)
library(caret)
library(randomForest)
library(ggplot2)
library(here)

set.seed(42)

df <- readRDS(here("data", "processed", "all_listings.rds"))

# ── 1. Build modelling dataframe ──────────────────────────────
model_df <- df %>%
  filter(listing_type == "satilik") %>%
  transmute(
    price             = as.numeric(price),
    GrossSquareMeters = as.numeric(GrossSquareMeters),
    room_count        = as.numeric(room_count),
    numberOfBathrooms = as.numeric(numberOfBathrooms),
    buildingAge       = as.numeric(buildingAge),
    Elevator          = as.logical(Elevator_lgl),
    Parking           = as.logical(Parking_lgl),
    InsideTheSite     = as.logical(InsideTheSite_lgl),
    HeatingType       = as.character(HeatingType),
    district          = as.character(district)
  ) %>%
  filter(!is.na(price), !is.na(GrossSquareMeters), !is.na(room_count),
         price > 0, GrossSquareMeters > 0) %>%
  mutate(
    log_price         = log(price),
    numberOfBathrooms = ifelse(is.na(numberOfBathrooms),
                               median(numberOfBathrooms, na.rm=TRUE), numberOfBathrooms),
    buildingAge       = ifelse(is.na(buildingAge),
                               median(buildingAge, na.rm=TRUE), buildingAge),
    Elevator      = ifelse(is.na(Elevator),     FALSE, Elevator),
    Parking       = ifelse(is.na(Parking),      FALSE, Parking),
    InsideTheSite = ifelse(is.na(InsideTheSite),FALSE, InsideTheSite),
    HeatingType   = factor(ifelse(is.na(HeatingType)|HeatingType=="NA",
                                  "Bilinmiyor", HeatingType)),
    district      = factor(ifelse(is.na(district)|district=="NA",
                                  "Bilinmiyor", district))
  ) %>%
  group_by(district) %>%
  filter(n() >= 5) %>%
  ungroup()

cat("Modelling dataset:", nrow(model_df), "rows\n")

RF_FEATURES <- c("GrossSquareMeters", "room_count", "numberOfBathrooms",
                 "buildingAge", "Elevator", "Parking", "InsideTheSite",
                 "HeatingType", "district")

# ── 2. Train / test split ─────────────────────────────────────
train_idx <- createDataPartition(model_df$log_price, p = 0.8, list = FALSE)
train_df  <- model_df[ train_idx, ]
test_df   <- model_df[-train_idx, ]

cat("Train:", nrow(train_df), "  Test:", nrow(test_df), "\n")

# ── 3. Train Random Forest ────────────────────────────────────
cat("\nTraining Random Forest (ntree=300) — takes ~2–5 min …\n")

rf_model <- randomForest(
  x          = train_df[, RF_FEATURES],
  y          = train_df$log_price,
  ntree      = 300,
  mtry       = max(1, floor(sqrt(length(RF_FEATURES)))),
  importance = TRUE,
  do.trace   = 100
)

print(rf_model)

# ── 4. Evaluate ───────────────────────────────────────────────
test_df$pred_log   <- predict(rf_model, newdata = test_df[, RF_FEATURES])
test_df$pred_price <- exp(test_df$pred_log)

rmse <- sqrt(mean((test_df$price - test_df$pred_price)^2, na.rm=TRUE))
mae  <- mean(abs(test_df$price  - test_df$pred_price),    na.rm=TRUE)
r2   <- cor(test_df$price, test_df$pred_price, use="complete.obs")^2

cat("\n=== RF MODEL EVALUATION ===\n")
cat(sprintf("RMSE : %s TL\n",  format(round(rmse), big.mark=",")))
cat(sprintf("MAE  : %s TL\n",  format(round(mae),  big.mark=",")))
cat(sprintf("R²   : %.4f\n",   r2))

# ── 5. Feature importance plot ────────────────────────────────
plot_dir <- here("visuals", "regression_plots")
dir.create(plot_dir, recursive=TRUE, showWarnings=FALSE)

imp <- as.data.frame(importance(rf_model)) %>%
  tibble::rownames_to_column("feature") %>%
  arrange(desc(`%IncMSE`))

p_fi <- ggplot(imp, aes(x=reorder(feature,`%IncMSE`), y=`%IncMSE`)) +
  geom_col(fill="#FF5722", alpha=0.85) +
  coord_flip() +
  labs(title="Random Forest — Feature Importance",
       subtitle="% Increase in MSE when feature is permuted",
       x=NULL, y="% Increase in MSE") +
  theme_minimal(base_size=13)

ggsave(file.path(plot_dir,"rf_feature_importance.png"),
       p_fi, width=9, height=6, dpi=150)
cat("Saved: visuals/regression_plots/rf_feature_importance.png\n")

# ── 6. Save ───────────────────────────────────────────────────
models_dir <- here("models")
dir.create(models_dir, recursive=TRUE, showWarnings=FALSE)

saveRDS(rf_model,      file.path(models_dir, "random_forest_model.rds"))
saveRDS(list(features=RF_FEATURES),
        file.path(models_dir, "rf_features.rds"))
saveRDS(data.frame(model="Random Forest", RMSE=rmse, MAE=mae, R2=r2),
        file.path(models_dir, "rf_metrics.rds"))

cat("\n✓ Random Forest model saved.\n")
cat("Next: source('r-analysis/08_model_comparison.R')\n")