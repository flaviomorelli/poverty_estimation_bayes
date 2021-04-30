library(loo)
library(tidyverse)
library(rstan)

options(mc.cores = parallel::detectCores())
# rstan_options(auto_write = TRUE)

N <- 500
x <- rnorm(N)
y <-  2 + 10 * x + rnorm(N, 0, 0.01)

stan_data_normal <- list(N = N, x = x, y = y)
stan_data_cold <- stan_data_normal
stan_data_normal$temperature <- 1
stan_data_cold$temperature <- 0.1

model <- stan_model("notebooks/cold_posterior/temperature_model.stan")

normal_chain <- sampling(model, stan_data_normal)
cold_chain <- sampling(model, stan_data_cold)

y_pred_normal <- extract(normal_chain, pars = "y_pred") %>% data.frame %>% t %>% data.frame
y_pred_cold <- extract(cold_chain, pars = "y_pred") %>% data.frame %>% t %>% data.frame

normal_rmse <- apply(y_pred_normal, MARGIN = 2, function(x) sqrt(mean((x - y)^2))) %>% as.numeric
cold_rmse <- apply(y_pred_cold, MARGIN = 2, function(x) sqrt(mean((x - y)^2))) %>% as.numeric

quantile(normal_rmse)
quantile(cold_rmse)

mean(normal_rmse)
mean(cold_rmse)

ggplot() + 
  geom_density(data = data.frame(normal=normal_rmse), aes(normal_rmse), color="blue") +
  geom_density(data = data.frame(normal=cold_rmse), aes(cold_rmse)) +
  theme_minimal()
