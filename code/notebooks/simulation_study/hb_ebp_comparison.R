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


log_shift_model <- cmdstan_model(model_path)
gq_model <- cmdstan_model(gq_path)

# Logscale scenario
logscale_fit <- log_shift_model$sample(
  logscale_smp_stan,
  chains = 2, 
  parallel_chains = 2
)

logscale_pred <- gq_model$generate_quantities(
  logscale_fit, 
  data = logscale_pop_stan, 
  seed = seed, 
  parallel_chains = 2
  )$draws() %>% 
  as_draws_matrix()

count_extremes(logscale_pop$y, logscale_pred, mode = "min")

# GB2 scenario
gb2_fit <- log_shift_model$sample(gb2_smp_stan, 
                  chains = 2,
                  parallel_chains = 2)

gb2_pred <- gq_model$generate_quantities(
                            gb2_fit, 
                            data = gb2_pop_stan, 
                            seed = seed, 
                            parallel_chains = 2
                            )$draws() %>% 
            as_draws_matrix()

count_extremes(gb2_pop$y, gb2_pred, mode = "max")

gb2_pred_impute <- impute_max(gb2_pop$y, gb2_pred)

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
pareto_fit <- log_shift_model$sample(pareto_smp_stan,
                                     chains = 2,
                                     parallel_chains = 2)

pareto_pred <- gq_model$generate_quantities(
  pareto_fit, 
  data = pareto_pop_stan, 
  seed = seed, 
  parallel_chains = 2
  )$draws() %>% 
  as_draws_matrix()

count_extremes(pareto_pop$y, pareto_pred, mode = "max")
pareto_pred_impute <- impute_max(pareto_pop$y, pareto_pred)

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

ppc_stat_2d(pareto_pop$y, 
            pareto_pred_impute, 
            stat = c("mean", "IQR"), 
            alpha = 0.3) 

mcmc_dens_chains(pareto_fit$draws("s"))

# EBP  

fixed_formula <- y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 

ebp_gb2 <- emdi::ebp(
          fixed_formula, 
          pop_data = gb2_pop, 
          pop_domains = "group_id", 
          smp_data = gb2_smp, 
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
  transformation = "box.cox",
  MSE = TRUE
)

