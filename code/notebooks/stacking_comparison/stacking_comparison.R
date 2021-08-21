library(tidyverse)
library(cmdstanr)

source(file.path("dataloader", "data_cleaning.R"))

model_fit <- function(model, data) {
  model$sample(data,
               chains = 4,
               parallel_chains = 4,
               iter_warmup = 1200, 
               iter_sampling = 400)
}

fit <- purrr::partial(model_fit, data = mcs_one_hot_strat)

loo_pointwise <- function(fit) fit$loo()$pointwise[, "elpd_loo"]

initial_mid_skewness_model <- cmdstan_model(
                                  file.path("model", "stan", "simulations", 
                                  "log_shift_mid_skewness.model.stan"))

initial_tight_skewness_model <- cmdstan_model(
                                  file.path("model", "stan", "simulations", 
                                  "log_shift_mid_skewness.model.stan"))

initial_wide_skewness_model <- cmdstan_model(
                                  file.path("model", "stan", "simulations", 
                                  "log_shift_mid_skewness.model.stan"))

raneff_base <- cmdstan_model(
                  file.path("model", "stan", "rand_eff_spec", 
                  "raneff_spec_base.model.stan"))

raneff_lkj <- cmdstan_model(
                  file.path("model", "stan", "rand_eff_spec", 
                  "raneff_corr.model.stan"))

raneff_sar <- cmdstan_model(
                  file.path("model", "stan", "rand_eff_spec", 
                  "raneff_corr_sar.model.stan"))

# horseshoe!

initial_mid_skewness_fit <- fit(initial_mid_skewness_model)
initial_tight_skewness_fit <- fit(initial_tight_skewness_model)
initial_wide_skewness_fit <- fit(initial_wide_skewness_model)
raneff_base_fit <- fit(raneff_base)
raneff_lkj_fit <- fit(raneff_lkj)
raneff_sar_fit <- fit(raneff_sar)

elpd_mid_skewness <- loo_pointwise(initial_mid_skewness_fit)
elpd_tight_skewness <- loo_pointwise(initial_tight_skewness_fit)
elpd_wide_skewness <- loo_pointwise(initial_wide_skewness_fit)
elpd_raneff_base <- loo_pointwise(raneff_base_fit)
elpd_raneff_lkj <- loo_pointwise(raneff_lkj_fit)
elpd_raneff_sar <- loo_pointwise(raneff_sar_fit)

