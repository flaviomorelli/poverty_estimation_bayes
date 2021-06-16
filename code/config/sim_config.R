seed <- 567

base_path <- file.path("model", "stan", "simulations")
model_path <- file.path(base_path, "log_shift_model.stan")
gq_path <- file.path(base_path, "log_shift_y_pred.stan")

rm(base_path)
