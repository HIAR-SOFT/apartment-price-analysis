# ============================================================
# 08_model_comparison.R
# Loads saved metrics and produces a comparison table + chart.
# ============================================================

library(dplyr)
library(ggplot2)
library(tidyr)

# Load metrics saved by 06 and 07
lm_m <- readRDS("models/lm_metrics.rds")
rf_m <- readRDS("models/rf_metrics.rds")

comparison <- bind_rows(lm_m, rf_m)
print(comparison)

# Visualise
comparison_long <- comparison %>%
  pivot_longer(cols = c(RMSE, MAE, R2), names_to = "metric", values_to = "value")

p <- ggplot(comparison_long, aes(x = model, y = value, fill = model)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~metric, scales = "free_y") +
  scale_fill_manual(values = c("Linear Regression" = "#2196F3", "Random Forest" = "#FF5722")) +
  labs(
    title = "Model Comparison: Linear Regression vs Random Forest",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(strip.text = element_text(face = "bold"))

ggsave("visuals/regression_plots/model_comparison.png", p, width = 10, height = 5, dpi = 150)
cat("Saved: visuals/regression_plots/model_comparison.png\n")

# Print interpretation
cat("\n=== INTERPRETATION ===\n")
best_rmse <- comparison$model[which.min(comparison$RMSE)]
best_r2   <- comparison$model[which.max(comparison$R2)]
cat("Best RMSE:", best_rmse, "\n")
cat("Best R²  :", best_r2, "\n")