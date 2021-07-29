library(cmdstanr)
library(tidyverse)
library(brms)
library(lme4)
library(nlme)

N <- 1000
D <- 10
E <- 5
group <- rep(1:D, each = N/D)
group_E <- rep(1:E, each = N/E)
u <- rnorm(D, 0, 2)
v <- rnorm(E, 0, 1)
x <- rnorm(N, 5, 3)
y <- rnorm(3 + u[group] + v[group_E]+ 5 * x, 0.2)

sim_data <- data.frame(x = x, y = y, group = group, group_E = group_E)

group_dummy <- matrix(nrow = N, ncol = D)
for(i in 1:D){
  group_dummy[ , i] <-  as.numeric(group == i)
}

data_dummies <- list(y = y,
                     X = cbind(x, group_dummy), 
                     N = N,
                     K = 1 + D)

data_intercept <- list(y = y,
                       X = matrix(x),
                       group = group,
                       D = D,
                       N = N,
                       K = 1)

dummies_model <- cmdstan_model("notebooks/dummies_check/dummies_model.stan")
intercepts_model <- cmdstan_model("notebooks/dummies_check/intercept_check.stan")
brms_model <- cmdstan_model("notebooks/dummies_check/brms_inspired.stan")

dummies_model$variational(data_dummies, 
                          seed = 123)

intercepts_model$variational(data_intercept, 
                             seed = 123)

brms_model$variational(data_intercept, 
                             seed = 123)


fit_dummies <- dummies_model$sample(data_dummies, 
                                    chains = 2, 
                                    parallel_chains = 2, 
                                    seed = 123, 
                                    refresh = 500)

fit_intercepts <- intercepts_model$sample(data_intercept, 
                                          chains = 2,
                                          parallel_chains = 2, 
                                          seed = 123, 
                                          refresh = 500)

fit_pseudo_brms <- brms_model$sample(data_intercept, 
                                          chains = 2,
                                          parallel_chains = 2, 
                                          seed = 123, 
                                          refresh = 500)

fit_dummies$summary()
fit_intercepts$summary()
fit_pseudo_brms$summary("reff")
fit_intercepts$cmdstan_diagnose()

lmer_fit <- lmer(y ~ x + (1|group), sim_data)
getME(lmer_fit, "Z") 

brm(y ~ x + (1|group) + (1|group_E), sim_data)
make_stancode(y ~ x + (1 |group), sim_data, family = student())
make_stancode(Reaction ~ Days + (Days || Subject), sleepstudy)


