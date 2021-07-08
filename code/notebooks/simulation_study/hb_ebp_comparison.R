library(tidyverse)
library(cmdstanr)
library(brms)

# Load configuration (seed, paths)
source(file.path("config", "sim_config.R"))

# Load simulations
source(file.path("dataloader", "load_simulations.R"))

# Load numeric functions
source(file.path("ops", "indicators.R"))

# Load helper functions for Stan
source(file.path("ops", "stan_helper.R"))

# Load graph functionality
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

hcr_pop <- indicator_pop_list(sim_data, scenarios, "hcr")
pgap_pop <- indicator_pop_list(sim_data, scenarios, "pgap")

hcr_sample <- indicator_list(sim_data, y_pred, scenarios, "hcr")
pgap_sample <- indicator_list(sim_data, y_pred, scenarios, "pgap")

hcr_hb <- hb_list(hcr_sample, scenarios)
pgap_hb <- hb_list(pgap_sample, scenarios)

create_graphs(hcr_pop, hcr_sample, scenarios,  "hcr")
create_graphs(pgap_pop, pgap_sample, scenarios, "pgap")

hcr_diagnostics <- diagnostics_list(hcr_sample, hcr_pop, scenarios)
pgap_diagnostics <- diagnostics_list(pgap_sample, pgap_pop, scenarios)

# EBP  

ebp_indicators <- ebp_indicators(sim_data, scenarios) 

compare_diagnostics(ebp_indicators, 
                    hcr_diagnostics, 
                    scenarios)

indicator_kde_graph(hcr_pop$pareto$pop,
                    ebp_indicators$pareto$smp$ind$Head_Count,
                    hcr_hb$pareto$smp) 
