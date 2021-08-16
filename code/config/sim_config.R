seed <- 567

base_path <- file.path("model", "stan", "simulations")
model_path <- file.path(base_path, "log_shift_mid_skewness.model.stan")
gq_path <- file.path(base_path, "log_shift_y_pred.stan")
gq_miss_path <- file.path(base_path, "log_shift_y_pred_miss.stan")

graph_path <- file.path("data", "simulations", "graphs")

rm(base_path)
