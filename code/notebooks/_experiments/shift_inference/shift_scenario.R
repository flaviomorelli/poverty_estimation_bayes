library(cmdstanr)
library(tidyverse)
library(posterior)
library(bayesplot)

source("ops/trafo.R")

set.seed(seed = 123)

# Quick simulation scenarios with Pareto error 
# and lognormal distribution

N <-  500

# The Student's t-distribution in a regressors
# changes makes the lognormal distribution more skewed
x <- rt(N, 3) 

y_pareto <- 130 + 10 * x + 0.7 * Pareto::rPareto(n = N, alpha = 0.7, t = 0.1)
min(y_pareto) > 0

y <- rlnorm(N, 10 + x * 0.4, 0.2)

moments::skewness(log(y))
moments::skewness(log(y_pareto))

# Shift terms by minimizing skewness
find_shift(y)$minimizer
find_shift(y_pareto)$minimizer

# Estimate the models

model <- cmdstan_model("notebooks/shift_inference/shift_model.stan")

# Lognormal scenario
fit_lnormal <- model$sample(list(y = y, x = x, N = N), 
                    chains = 2, 
                    parallel_chains = 4, 
                    iter_warmup = 1000,
                    adapt_delta = 0.8, 
                    max_treedepth = 13)

fit_lnormal
fit_lnormal$cmdstan_diagnose()

y_pred_lnormal <- fit_lnormal$draws("y_pred") %>% as_draws_matrix

hcr_lnormal <- laeken::arpr(y)
hcr_ypred_lnormal <- apply(X = y_pred_lnormal, 
      MARGIN = 1, 
      FUN = function(x) laeken::arpr(x)$value)

hcr_bias_lnormal <- hcr_ypred_lnormal - hcr_lnormal$value 

hist(hcr_ypred_lnormal)
hist(hcr_bias_lnormal)
median(hcr_bias_lnormal)

hcr_rmse_lnormal <- sqrt(mean((hcr_ypred_lnormal - hcr_lnormal$value)^2))

# Check skewness parameter
mcmc_dens(fit_lnormal$draws("s"))
mcmc_parcoord(fit_lnormal$draws(c("nu", "s", "sigma")) %>% 
                as_draws_matrix,
              np = nuts_params(fit_lnormal))
mcmc_pairs(fit_lnormal$draws(c("nu", "s", "sigma", "alpha", "beta")) %>% 
             as_draws_matrix,
           np = nuts_params(fit_lnormal))
mcmc_nuts_energy(nuts_params(fit_lnormal))

ppc_dens_overlay(y, y_pred_lnormal[1:100, ]) + 
  theme_minimal() +
  xlim(c(0, 200000))

# Pareto scenario
# The Pareto scenario is difficult, because the transformed
# variable requires a very low number of degrees of freedom. 
# If there is a hard lower bound on the df, then lots of the 
# MCMC proposals will be rejected, as the sampler tends toward
# regions below the lower bound

# The model with nu_raw (lower bound zero) produced
# much better results thatn if nu has a lower bound

# A warmup of 1000 iterations was insufficient
fit_pareto <- model$sample(list(y = y_pareto, x = x, N = N), 
                            chains = 2, 
                            parallel_chains = 2, 
                            iter_warmup = 1400,
                            iter_sampling = 800,  
                            adapt_delta = 0.9, 
                            max_treedepth = 13)

fit_pareto
fit_pareto$cmdstan_diagnose()

y_pred_pareto <- fit_pareto$draws("y_pred") %>% as_draws_matrix

hcr_pareto <- laeken::arpr(y_pareto)
hcr_ypred_pareto <- apply(X = y_pred_pareto, 
                           MARGIN = 1, 
                           FUN = function(x) laeken::arpr(x)$value)

hcr_bias_pareto <- hcr_ypred_pareto - hcr_pareto$value 

hist(hcr_ypred_pareto)
hist(hcr_bias_pareto)
median(hcr_bias_pareto)
mean(hcr_bias_pareto)
mean(hcr_ypred_pareto)

hcr_rmse_pareto <- sqrt(mean((hcr_ypred_pareto - hcr_pareto$value)^2))

# Check skewness parameter
mcmc_dens(fit_pareto$draws("s"))

# Even when all diagnostics look fine, 
# checking MCMC plots is crucial to make sure that the diagnostics
# are accurate, but also to find out what parameters 
# are causing problem to the model 
mcmc_parcoord(fit_pareto$draws(c("s", "sigma")) %>% 
                as_draws_matrix,
              np = nuts_params(fit_pareto))

mcmc_pairs(fit_pareto$draws(c("s", "sigma", "nu")),
           np = nuts_params(fit_pareto)) 

ppc_dens_overlay(y_pareto, y_pred_pareto[1:100, ]) +
  xlim(c(0, 250))

mcmc_scatter(
  fit_pareto$draws(c("nu", "s")) %>% 
    as_draws_matrix,
  np = nuts_params(fit_pareto),
)

mcmc_trace(fit_pareto$draws(c("s", "sigma")) ) 

mcmc_nuts_energy(nuts_params(fit_pareto))

mcmc_acf(fit_pareto$draws(c("s", "alpha", "nu")), 
         lags = 20)
