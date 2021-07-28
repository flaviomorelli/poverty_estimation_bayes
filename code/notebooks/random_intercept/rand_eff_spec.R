library(tidyverse)
library(tidymodels)
library(cmdstanr)
library(bayesplot)
library(posterior)
library(brms)
library(loo)

# Load data frames and lists
source(file.path("dataloader", "data_cleaning.R"))
rm(census, census_one_hot, mcs, mcs_stan)

model <- cmdstan_model(file.path("model", "stan", "rand_eff_spec", 
                                 "raneff_spec.model.stan"))

# model$variational(mcs_one_hot_mun, iter = 20000)

mun_draws <- model$sample(mcs_one_hot_mun, 
                          chains = 4, 
                          iter_warmup = 1000,
                          iter_sampling = 500,
                          parallel_chains = 4,
                          max_treedepth = 13, 
                          adapt_delta = 0.85)

# model$variational(mcs_one_hot_strat, iter = 20000)

strat_draws <- model$sample(mcs_one_hot_strat, 
                          chains = 4, 
                          iter_warmup = 1000, 
                          iter_sampling = 500,
                          parallel_chains = 4,
                          max_treedepth = 13, 
                          adapt_delta = 0.85)

loo_mun <- mun_draws$loo()
# Estimate    SE
# elpd_loo -14892.4  50.8
# p_loo        30.6   0.7
# looic     29784.7 101.6

loo_strat <- strat_draws$loo()
# Estimate    SE
# elpd_loo -14855.5  50.7
# p_loo        30.1   0.8
# looic     29711.1 101.3

loo_compare(loo_mun, loo_strat)

# elpd_diff se_diff
# model2   0.0       0.0  
# model1 -36.8      11.3  

mun_draws$draws("s") %>% mcmc_dens()
