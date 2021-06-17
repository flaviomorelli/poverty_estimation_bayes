library(tidyverse)
library(cmdstanr)
library(bayesplot)
library(posterior)

# Load configuration (seed, paths)
source(file.path("config", "sim_config.R"))

# Load simulations
source(file.path("dataloader", "load_simulations.R"))

# Load numeric functions
source(file.path("ops", "indicators.R"))
source(file.path("ops", "imputation.R"))

# Load helper functions for Stan
source(file.path("ops", "stan_helper.R"))

# Setup

list(logscale = "logscale", gb2 = "gb2", pareto = "pareto")

log_shift_model <- cmdstan_model(model_path)
gq_model <- cmdstan_model(gq_path)
gq_miss_model <- cmdstan_model(gq_miss_path)


fit <- lapply(scenarios,
              scenario_fit, 
              data = sim_data, 
              model = log_shift_model)

lapply(unlist(fit), function(x) x$cmdstan_diagnose())
                            
# Logscale scenario


logscale_pred <- get_pred(gq_model, 
                          fit$logscale$smp, 
                          pop = sim_data$logscale$pop_stan, 
                          smp = sim_data$logscale$smp_stan) 

logscale_miss_pred <- get_pred(gq_miss_model, 
                                fit$logscale$smp_miss, 
                                pop = sim_data$logscale$pop_stan,
                                smp = sim_data$logscale$smp_miss_stan, 
                                type = "missing") 
                                    

# Script: Generate plots
ppc_dens_overlay(sim_data$logscale$pop$y, logscale_miss_pred[1:100, ])
ppc_stat_2d(sim_data$logscale$pop$y, 
            logscale_miss_pred, 
            stat = c("median", "mean"), alpha = 0.3)
ppc_stat_2d(logscale_pop$y, logscale_pred, stat = c("median", "IQR"))

# GB2 scenario
gb2_fit <- log_shift_model$sample(gb2_smp_stan, 
                  chains = 2,
                  parallel_chains = 2)

gb2_miss_fit <- log_shift_model$sample(
  gb2_smp_miss_stan,
  chains = 2, 
  parallel_chains = 2
)

gb2_pred <- gq_model$generate_quantities(
                            gb2_fit, 
                            data = gb2_pop_stan, 
                            seed = seed, 
                            parallel_chains = 2
                            )$draws() %>% 
            as_draws_matrix()

extremes_diagnostics(gb2_pop$y, gb2_pred)

gb2_miss_pred <- gq_miss_model$generate_quantities(
  gb2_miss_fit, 
  data = pop_data_miss(gb2_pop_stan, gb2_smp_miss_stan), 
  seed = seed, 
  parallel_chains = 2
  )$draws("y_pred") %>% 
  as_draws_matrix()

gb2_pred_imp <- impute_max(gb2_pop$y, gb2_pred)

gb2_miss_pred_imp <- impute_max(gb2_smp_miss$y, 
                                     gb2_miss_pred)

ppc_dens_overlay(gb2_pop$y, gb2_miss_pred_imp[1:100, ])
ppc_stat_2d(gb2_pop$y, gb2_miss_pred_imp, stat = c("median", "mean"))
ppc_stat_2d(gb2_pop$y, gb2_pred_imp, stat = c("median", "mean"))


hcr_gb2_pop <- hcr(gb2_pop$y, 
    group = factor(gb2_pop$group_id), 
    t = poverty_line(gb2_pop$y))

hcr_gb2_pred <- fgt(t(gb2_pred_impute), 
    factor(gb2_pop$group_id), 
    max(gb2_pop$group_id))

hcr_hb_gb2 <- hb_indicator(hcr_gb2_pred)
hcr_diagnostics_gb2 <- indicator_diagnostics(hcr_gb2_pred, hcr_gb2_pop)

ppc_stat_2d(gb2_pop$y, 
            gb2_pred_impute, 
            stat = c("mean", "sd"), 
            alpha = 0.3) 

ppc_stat_grouped(gb2_pop$y, 
            gb2_pred_impute,
            group = gb2_pop$group_id,
            stat = "median") 

ppc_dens_overlay(gb2_pop$y, 
                 gb2_pred_impute[1:100, ])


# Pareto scenario
pareto_fit <- log_shift_model$sample(
  pareto_smp_stan,
  chains = 2,
  parallel_chains = 2
  )


pareto_pred <- gq_model$generate_quantities(
  pareto_fit, 
  data = pareto_pop_stan, 
  seed = seed, 
  parallel_chains = 2
  )$draws() %>% 
  as_draws_matrix()

pareto_pred_smp <- gq_model$generate_quantities(
  pareto_fit, 
  data = pareto_smp_stan, 
  seed = seed, 
  parallel_chains = 2
  )$draws() %>% 
  as_draws_matrix()

extremes_diagnostics(pareto_pop$y, pareto_pred)
pareto_pred_impute <- impute_max(pareto_smp$y, pareto_pred)
pareto_pred_impute_smp <- impute_max(pareto_smp$y, pareto_pred_smp)

hcr_pareto_pop <- hcr(pareto_pop$y, 
                   group = factor(pareto_pop$group_id), 
                   t = poverty_line(pareto_pop$y))

hcr_pareto_pred <- fgt(t(pareto_pred_impute), 
                    factor(pareto_pop$group_id), 
                    max(pareto_pop$group_id))

hcr_hb_pareto <- hb_indicator(hcr_pareto_pred)
hcr_diagnostics_pareto <- indicator_diagnostics(hcr_pareto_pred, hcr_pareto_pop)

ppc_dens_overlay(pareto_pop$y, 
                 pareto_pred_impute[1:100, ])

ppc_dens_overlay(pareto_smp$y, 
                 pareto_pred_impute_smp[1:100, ])

ppc_stat_2d(pareto_smp$y, 
            pareto_pred_impute_smp, 
            stat = c("mean", "IQR"), 
            alpha = 0.3) 

mcmc_dens_chains(pareto_fit$draws("s"))

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

