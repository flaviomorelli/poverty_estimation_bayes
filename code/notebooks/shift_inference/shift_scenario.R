library(cmdstanr)

N <-  1000

x <- rnorm(N, 20, 3)

y_pareto <- 2 + 10 * x + 0.7 * Pareto::rPareto(n = N, alpha = 0.7, t = 0.1)

y <- rlnorm(N, 0.5 + x * 0.4, 0.2)

hist(y_pareto)
hist(log(y_pareto))
moments::skewness(y_pareto)
moments::skewness(log(y_pareto - 2))

abs_skewness <- function(s, x) abs(moments::skewness(log(x + s)))

stan_data <- list(y = y_pareto, x = x, N = N)

model <- cmdstan_model("notebooks/shift_inference/shift_model.stan")

fit <- model$sample(stan_data, chains = 2, parallel_chains = 2)
fit$summary()
fit$cmdstan_diagnose()

optimize(abs_skewness, interval = c(-min(y_pareto), 0), x = y_pareto)
