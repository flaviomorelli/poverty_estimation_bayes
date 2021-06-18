library(tidyverse)
library(cmdstanr)

# Load configuration (seed, paths)
source(file.path("config", "sim_config.R"))

# Load simulations
source(file.path("dataloader", "load_simulations.R"))

# Load numeric functions
source(file.path("ops", "indicators.R"))

# Load helper functions for Stan
source(file.path("ops", "stan_helper.R"))

# Load
source(file.path("utils", "graphics", "sim_graphs.R"))

# Setup
scenarios <- list(logscale = "logscale", gb2 = "gb2", pareto = "pareto")

log_shift_model <- cmdstan_model(model_path)
gq_model <- cmdstan_model(gq_path)
gq_miss_model <- cmdstan_model(gq_miss_path)

# Fit models and get predictions
fit <- lapply(scenarios,
              scenario_fit, 
              data = sim_data, 
              model = log_shift_model)
                            
y_pred <- scenario_pred_list(sim_data, 
                             gq_model,
                             gq_miss_model,
                             fit, 
                             scenarios)

create_graphs(sim_data, y_pred, scenarios)
skewness_graphs(fit, scenarios)

# Script to calculate indicators
hcr_gb2_pop <- hcr(gb2_pop$y, 
    group = factor(gb2_pop$group_id), 
    t = poverty_line(gb2_pop$y))

hcr_gb2_pred <- fgt(t(gb2_pred_impute), 
    factor(gb2_pop$group_id), 
    max(gb2_pop$group_id))

hcr_hb_gb2 <- hb_indicator(hcr_gb2_pred)
hcr_diagnostics_gb2 <- indicator_diagnostics(hcr_gb2_pred, hcr_gb2_pop)


hcr_pareto_pop <- hcr(pareto_pop$y, 
                   group = factor(pareto_pop$group_id), 
                   t = poverty_line(pareto_pop$y))

hcr_pareto_pred <- fgt(t(pareto_pred_impute), 
                    factor(pareto_pop$group_id), 
                    max(pareto_pop$group_id))

hcr_hb_pareto <- hb_indicator(hcr_pareto_pred)
hcr_diagnostics_pareto <- indicator_diagnostics(hcr_pareto_pred, hcr_pareto_pop)

# EBP  

fixed_formula <- y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 

ebp_gb2 <- emdi::ebp(
  fixed_formula, 
  pop_data = sim_data$gb2$pop, 
  pop_domains = "group_id", 
  smp_data = sim_data$gb2$smp_miss, 
  smp_domains = "group_id",
  transformation = "box.cox",
  MSE = TRUE
  )

ebp_pareto <- emdi::ebp(
  fixed_formula, 
  pop_data = pareto_pop, 
  pop_domains = "group_id", 
  smp_data = pareto_smp, 
  smp_domains = "group_id",
  transformation = "log",
  MSE = TRUE, 
  boot_type = "wild"
)

