library(tidyverse)
library(brms)

# Load simulations
source(file.path("dataloader", "load_simulations.R"))

fixed_formula <- y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + (1|group_id)

exgaussian_fit <- brm(fixed_formula, 
    data = sim_data$gb2$smp, 
    family = exgaussian(), 
    prior = c(prior(normal(0, 200), class = b),
              prior(gamma(3, 1), class = beta)), 
    warmup = 3000, 
    iter = 3500,
    cores = 4)

make_stancode(fixed_formula, 
    data = sim_data$gb2$smp, 
    family = exgaussian(), 
    prior = c(prior(normal(0, 200), class = b)), 
    cores = 4)

mcmc_plot(exgaussian_fit, type = "dens")


