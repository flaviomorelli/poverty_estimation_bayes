library(rstan)
library(tidyverse)
library(loo)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

N <-  1000

x <- rnorm(N, 20, 3)

y_pareto <- 2 + 10 * x + 0.7 * Pareto::rPareto(n = N, alpha = 0.7, t = 0.001)

y <- rlnorm(N, 0.5 + x * 0.4, 0.2)

hist(y)
hist(log(y))
moments::skewness(y)
moments::skewness(log(y))

skew_model <- stan_model("notebooks/stan/skew_normal_regression.stan")
t_model <- stan_model("notebooks/stan/t_regression.stan")
stan_data <-  list(N = N, x = x, y = y, K = 1)
stan_data_log <- list(N = N, x = x, y = log(y), K = 1)

fit <- sampling(skew_model, stan_data, iter = 2000, chains = 2)
fit_t <- sampling(t_model, stan_data_log, iter = 2000, chains = 2)

log_lik_skew <- extract_log_lik(fit, merge_chains = FALSE)
r_eff_skew <- relative_eff(exp(log_lik_skew), cores = 2)
loo_skew <- loo(log_lik_skew, r_eff = r_eff_skew, cores = 2)

log_lik_t <- extract_log_lik(fit_t, merge_chains = FALSE)
r_eff_t <- relative_eff(exp(log_lik_t), cores = 2)
loo_t <- loo(log_lik_t, r_eff = r_eff_t, cores = 2)

loo_compare(loo_skew, loo_t)

skew_pred <- rstan::extract(fit, pars = "y_pred", ) %>% data.frame %>% 
      sample_n(100) %>% t %>% data.frame
t_pred <- rstan::extract(fit_t, pars = "y_pred") %>% data.frame %>% 
      sample_n(100) %>% t %>% data.frame

write_csv(skew_pred %>% gather, "notebooks/data/skew_pred.csv")
write_csv(t_pred %>% gather, "notebooks/data/t_pred.csv")
write_csv(data.frame(y = y), "notebooks/data/y.csv")

y <- read_csv("notebooks/data/y.csv")
s <- read_csv("notebooks/data/skew_pred.csv")
t <- read_csv("notebooks/data/t_pred.csv")
t$value <-  exp(t$value)
#t$value <- ifelse(t$value > 1.05 * max(y), 
#                  runif(1, 0.98 * max(y), 1.05 * max(y)), 
#                  t$value)


rmse_skew <- ((rep(y, times = 100) %>% unlist) - s$value)^2 %>% 
  matrix(ncol = 20) %>% 
  apply(MARGIN = 2, FUN = mean) %>% 
  sqrt

rmse_t <- ((rep(y, times = 100) %>% unlist) - t$value)^2 %>% 
  matrix(ncol = 20) %>% 
  apply(MARGIN = 2, FUN = mean) %>% 
  sqrt

par(mfrow = c(2, 1))
hist(mse_skew)
hist(mse_t)

ggplot() + 
  geom_density(aes(x = value, group = key), 
               color = "#cccccc",
               data = s) + 
  geom_density(aes(x = y), data.frame(y = y)) +
  theme_minimal()

ggplot() + 
  geom_density(aes(x = value, group = key), 
               color = "#cccccc",
               data = t) + 
  geom_density(aes(x = y), data.frame(y = y)) +
  theme_minimal()
