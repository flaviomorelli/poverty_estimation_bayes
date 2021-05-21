library(cmdstanr)
library(tidyverse)
library(bayesplot)

data("wage1", package = "wooldridge")
 
N <-  1000
set.seed(seed = 123)
x <- rnorm(N, 20, 3)

y_pareto <- -105 + 10 * x + 0.7 * Pareto::rPareto(n = N, alpha = 0.7, t = 0.1)
min(y_pareto)

y <- rlnorm(N, 0.5 + x * 0.4, 0.2)

hist(y)
hist(log(y))
moments::skewness(log(y))


hist(y_pareto)
plot(density(log(y_pareto - 10)))
hist(log(y_pareto-0.1037656))
moments::skewness(y_pareto)
moments::skewness(log(y_pareto))
moments::skewness(log(y_pareto  + 6))

log(wage1$wage) %>% hist

abs_skewness <- function(s, x){
  target <- log(x + s)
  moments::skewness(target)
}

stan_data <- list(y = y_pareto, x = x, N = N)

model <- cmdstan_model("notebooks/shift_inference/shift_model.stan")

model$variational(stan_data)

fit <- model$sample(stan_data, chains = 2, parallel_chains = 2, 
                    iter_warmup = 1000, adapt_delta = 0.85)


fit$summary()
fit$cmdstan_diagnose()

mcmc_trace(fit$draws("lambda"))
mcmc_nuts_divergence(fit$draws("lambda"))

optimize(abs_skewness, interval = c(-min(y_pareto), 100), x = y_pareto)

log(wage1$wage + -0.4) %>% moments::kurtosis()

