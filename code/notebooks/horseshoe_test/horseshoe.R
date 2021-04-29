library(rstan)
library(tidyverse)
library(loo)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

N <- 200
K <- 4
X <- rnorm(N * K) %>% matrix(ncol = 4) %>% data.frame

y <- X$X1 + 0.3 * X$X3 - 0.1 * X$X4 + rnorm(N, 0, 0.1) 

standardize <- function(x) (x - mean(x)) / sd(x)

stan_data <- list(y = y, X = X, N = N, K = K)

horseshoe_model <-  stan_model("notebooks/horseshoe_test/horshoe_prior.stan")
normal_model <-  stan_model("notebooks/horseshoe_test/normal_prior.stan")

hs_chains <- sampling(horseshoe_model, stan_data, chains = 2, iter = 2000, 
                      control = list(adapt_delta = 0.99))
n_chains <- sampling(normal_model, stan_data, chains = 2, iter = 2000)

loglik_hs <- extract_log_lik(hs_chains)
loglik_n <- extract_log_lik(n_chains)

loo_hs <- loo(loglik_hs, relative_eff(exp(loglik_hs)))
loo_n <- loo(loglik_n, relative_eff(exp(loglik_n)))
