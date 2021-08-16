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
              model = log_shift_model, 
              chains = 4, 
              iter_warmup = 1200, 
              iter_sampling = 300)
                            
y_pred <- scenario_pred_list(sim_data, 
                             gq_model,
                             gq_miss_model,
                             fit, 
                             scenarios)

create_graphs(sim_data, y_pred, scenarios, graph_path = graph_path)
skewness_graphs(fit, scenarios)

hcr_pop <- indicator_pop_list(sim_data, scenarios, "hcr")
pgap_pop <- indicator_pop_list(sim_data, scenarios, "pgap")

hcr_sample <- indicator_list(sim_data, y_pred, scenarios, "hcr")
pgap_sample <- indicator_list(sim_data, y_pred, scenarios, "pgap")

hcr_hb <- hb_list(hcr_sample, scenarios)
pgap_hb <- hb_list(pgap_sample, scenarios)

# create_graphs(hcr_pop, hcr_sample, scenarios,  "hcr")
# create_graphs(pgap_pop, pgap_sample, scenarios, "pgap")

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

# Create bad skewness plots

bad_model <- cmdstan_model(file.path("model", "stan", "simulations", 
                                     "log_shift_wide_skewness.model.stan"))

bad_fit_logscale <- bad_model$sample(sim_data$logscale$smp_stan, 
                                parallel_chains = 4,
                                iter_warmup = 1200,
                                iter_sampling = 300)

bad_fit_gb2 <- bad_model$sample(sim_data$gb2$smp_stan, 
                            parallel_chains = 4,
                            iter_warmup = 1200,
                            iter_sampling = 300)

bad_fit_pareto <- bad_model$sample(sim_data$pareto$smp_stan, 
                                parallel_chains = 4,
                                iter_warmup = 1200,
                                iter_sampling = 300)

bad_fit_skewness <- bad_fit_gb2$draws() %>% mcmc_dens(pars = "s") + xlab("")

tight_model <- cmdstan_model(file.path("model", "stan", "simulations", 
                                     "log_shift_tight_skewness.model.stan"))
tight_fit_logscale <- tight_model$sample(sim_data$logscale$smp_stan, 
                                      parallel_chains = 4,
                                      iter_warmup = 1200,
                                      iter_sampling = 300)

tight_fit_gb2 <- tight_model$sample(sim_data$gb2$smp_stan, 
                            parallel_chains = 4,
                            iter_warmup = 1200,
                            iter_sampling = 300)

tight_fit_pareto <- tight_model$sample(sim_data$pareto$smp_stan, 
                                      parallel_chains = 4,
                                      iter_warmup = 1200,
                                      iter_sampling = 300)

tight_skewness <- tight_fit_gb2$draws() %>% mcmc_dens(pars = "s") + xlab("")

ggsave(file.path("notebooks", "simulation_study", "bad_skewness.png"), 
         plot = bad_fit_skewness, 
         width = 8,
         height = 7, 
         units = "cm")

ggsave(file.path("notebooks", "simulation_study", "tight_skewness.png"), 
       plot = tight_skewness, 
       width = 8,
       height = 7, 
       units = "cm")

gb2_skewness <- fit$gb2$smp$draws() %>% mcmc_dens(pars = "s") + xlab("")
ggsave(file.path("notebooks", "simulation_study", "gb2_skewness.png"), 
       plot = gb2_skewness, 
       width = 8,
       height = 7, 
       units = "cm")

loo::loo_compare(tight_fit_logscale$loo(), 
                 fit$logscale$smp$loo())

loo::loo_compare(tight_fit_gb2$loo(), 
                 fit$gb2$smp$loo())

loo::loo_compare(tight_fit_pareto$loo(), 
                 fit$pareto$smp$loo())

