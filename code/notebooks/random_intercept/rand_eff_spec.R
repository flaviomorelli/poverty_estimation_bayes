library(tidyverse)
library(cmdstanr)
library(bayesplot)
library(posterior)
library(loo)

# Load data frames and lists
source(file.path("dataloader", "data_cleaning.R"))
rm(census, census_one_hot, mcs, mcs_stan)

# Are there out-of-sample areas?
mcs_one_hot %>% group_by(strat_idx) %>% count 

# Print quantiles for number of observations 
mcs_one_hot %>% group_by(strat_idx) %>% count %>% .[["n"]] %>% quantile

model <- cmdstan_model(file.path("model", "stan", "rand_eff_spec", 
                                 "raneff_spec.model.stan"))

mun_draws <- model$sample(mcs_one_hot_mun, 
                          chains = 4, 
                          iter_warmup = 1200,
                          iter_sampling = 300,
                          parallel_chains = 4)

# model$variational(mcs_one_hot_strat, iter = 20000)

strat_draws <- model$sample(mcs_one_hot_strat, 
                          chains = 4, 
                          iter_warmup = 1200, 
                          iter_sampling = 300,
                          parallel_chains = 4)
                         

loo_mun <- mun_draws$loo()
# Estimate    SE
# elpd_loo -14833.2  54.3
# p_loo        42.9   1.2
# looic     29666.3 108.7
# ------
# Monte Carlo SE of elpd_loo is 0.2.

loo_strat <- strat_draws$loo()
# Estimate    SE
# elpd_loo -14799.1  54.4
# p_loo        41.2   1.3
# looic     29598.3 108.8
# ------
# Monte Carlo SE of elpd_loo is 0.2.

loo_compare(loo_mun, loo_strat)

# elpd_diff se_diff
# model2   0.0       0.0  
# model1 -34.8      12.8  

mun_draws$draws("s") %>% mcmc_dens()
