
# 08_model_comparison.R

library(dplyr)
library(ggplot2)
library(tidyr)
library(here)

lm_path <- here("models", "lm_metrics.rds")
rf_path <- here("models", "rf_metrics.rds")

if (!file.exists(lm_path) || !file.exists(rf_path)) {
  stop("Run 06_regression_model.R and 07_random_forest.R first.")
}

comparison <- bind_rows(readRDS(lm_path), readRDS(rf_path))

cat("MODEL COMPARISON \n")
print(comparison)

long <- comparison %>%
  pivot_longer(c(RMSE, MAE, R2), names_to = "metric", values_to = "value")

p <- ggplot(long, aes(x = model, y = value, fill = model)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~metric, scales = "free_y") +
  scale_fill_manual(values = c(
    "Linear Regression" = "#2196F3",
    "Random Forest"     = "#FF5722"
  )) +
  labs(
    title    = "Model Comparison: Linear Regression vs Random Forest",
    subtitle = "Istanbul Apartment Prices 2026",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(strip.text = element_text(face = "bold"))

plot_dir <- here("visuals", "regression_plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(plot_dir, "model_comparison.png"), p, width = 10, height = 5, dpi = 150)
cat("Saved: visuals/regression_plots/model_comparison.png\n")

cat("\n INTERPRETATION \n")
cat("Best RMSE:", comparison$model[which.min(comparison$RMSE)], "\n")
cat("Best R²  :", comparison$model[which.max(comparison$R2)],   "\n")
cat("\n Comparison done! Ready to launch Shiny app:\n")
cat("  shiny::runApp('shiny-app')\n")