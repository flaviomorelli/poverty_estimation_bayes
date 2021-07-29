library(loo)
library(tidyverse)
library(rstan)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
 
milk <-  milk[complete.cases(milk), ]

clade <- as.numeric(milk$clade)

X <- milk %>% 
  select(-species, -kcal.per.g, -clade)

stan_data <- list(X = X,
                  y = milk$kcal.per.g,
                  N = nrow(X), 
                  K = ncol(X), 
                  group = clade,
                  n_group = 4)

model_multiple <- stan_model("notebooks/stacking_test/model_multiple.stan")
model_constant <- stan_model("notebooks/stacking_test/model_c.stan")
model_rand_eff <- stan_model("notebooks/stacking_test/model_rand_eff.stan")

multiple_chains <- sampling(model_multiple, stan_data, chains = 4, iter = 2000)
constant_chains <- sampling(model_constant, stan_data, chains = 4, iter = 2000)
rand_eff_chains <- sampling(model_rand_eff, stan_data, chains = 4, iter = 2000)

loo_rand_eff <- extract_log_lik(rand_eff_chains) %>% loo
loo_constant <- extract_log_lik(constant_chains) %>% loo
loo_multiple <- extract_log_lik(multiple_chains) %>% loo

loo_compare(loo_rand_eff, loo_constant, loo_multiple)

lpd_point <- cbind(
  loo_rand_eff$pointwise[,"elpd_loo"],
  loo_constant$pointwise[,"elpd_loo"],
  loo_multiple$pointwise[,"elpd_loo"]
)

stacking_weights(lpd_point)
