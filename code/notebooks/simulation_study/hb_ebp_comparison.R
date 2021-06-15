library(tidyverse)
library(cmdstanr)

source(file.path("dataloader", "load_simulations.R"))

seed <- 567

base_path <- file.path("model", "stan", "simulations")
model_path <- file.path(base_path, "log_shift_model.stan")
gq_path <- file.path(base_path, "log_shift_y_pred.stan")

stan_model <- cmdstan_model(model_path)
gq_model <- cmdstan_model(gq_path)

stan_model$variational(gb2_smp_stan)
gb2_fit <- stan_model$sample(gb2_smp_stan, 
                  chains = 2,
                  parallel_chains = 2)

gq_model$generate_quantities(gb2_fit, 
                             data = gb2_pop_stan, 
                             seed = seed, 
                             parallel_chains = 2)


fixed_formula <- y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 

ebp_gb2 <- emdi::ebp(fixed_formula, 
          pop_data = gb2_pop, 
          pop_domains = "group_id", 
          smp_data = gb2_smp, 
          smp_domains = "group_id",
          transformation = "log",
          MSE = TRUE)

brms::make_standata(fixed_formula, 
    data =  gb2_smp)

logscale_model <- update(gb2_model, newdata = logscale, cores = 2)
pareto_model <- update(gb2_model, newdata = pareto, cores = 2)

brms::pp_check(gb2_model)
min(gb2$y)


