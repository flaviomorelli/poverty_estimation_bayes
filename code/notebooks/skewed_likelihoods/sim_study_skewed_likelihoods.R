library(tidyverse)
library(brms)

# Load simulations
source(file.path("dataloader", "load_simulations.R"))
# Load helper functions for Stan
source(file.path("ops", "stan_helper.R"))
source(file.path("utils", "graphics", "sim_graphs.R"))

bayesplot::bayesplot_theme_set(
    theme_minimal(base_size = 23) + 
        theme(legend.position = "none")
    )

reg_formula <- y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + (1|group_id)
scenarios <- c("logscale", "gb2", "pareto")
types <- c("smp", "smp_miss")

# iteration_loop <- function(FUN, ...){
#     result <- list()
#     for(scenario in scenarios){
#         message(str_c("Scenario: ", scenario))
#         for(type in types){
#             result[[scenario]][[type]] <- FUN(scenario, type, ...)
#         }
#     }
#     return(result)
# }

brm_models <- function(scenario, 
                       type, 
                       family, 
                       prior = NULL,
                       chains = 4, 
                       iter = 1500, 
                       warmup = 1000, 
                       control = list(adapt_delta = 0.8)){
    brm(reg_formula, 
      data = sim_data[[scenario]][[type]],
      family = family, 
      prior = prior,
      iter = iter, 
      warmup = warmup,
      chains = chains, 
      control = control,
      cores = 4)
}

Q10 <- function(x) quantile(x, 0.1)
Q90 <- function(x) quantile(x, 0.9)

gamma_log_fit <- iteration_loop(brm_models, 
                                family = Gamma(link = "log"))

create_graphs(sim_data, gamma_log_fit, scenarios, 
             graph_path = file.path("notebooks", "skewed_likelihoods", "graphs"), 
             name = "gamma_log", from_brms = TRUE)



# Some R-hat values slightly high, but no divergences
gamma_softplus_fit <- iteration_loop(brm_models, 
                                     family = brmsfamily("gamma", link = "softplus"))
create_graphs(sim_data, gamma_softplus_fit, scenarios, 
              graph_path = file.path("notebooks", "skewed_likelihoods", "graphs"), 
              name = "gamma_softplus", from_brms = TRUE)


lognormal_fit <- iteration_loop(brm_models, 
                                family = lognormal())
create_graphs(sim_data, lognormal_fit, scenarios, 
              graph_path = file.path("notebooks", "skewed_likelihoods", "graphs"), 
              name = "lognormal", from_brms = TRUE)

# Pareto smp with very high R-square. Does not capture distribution
skewnormal_fit <- iteration_loop(brm_models, 
                                 family = skew_normal())
create_graphs(sim_data, skewnormal_fit, scenarios, 
              graph_path = file.path("notebooks", "skewed_likelihoods", "graphs"), 
              name = "skewnormal", from_brms = TRUE)


# Fitting problems. No model with small R-hat
exgaussian_fit <- 
    iteration_loop(brm_models, family = exgaussian(), 
     prior = prior(normal(0, 200), class = b),
     control = list(adapt_delta = 0.99),
     warmup = 3000, 
     iter = 3500)
create_graphs(sim_data, exgaussian_fit, scenarios, 
              graph_path = file.path("notebooks", "skewed_likelihoods", "graphs"), 
              name = "exgaussian", from_brms = TRUE)

# Check Stan code for Gamma likelihood
make_stancode(reg_formula, 
    data = sim_data[["gb2"]][["smp"]],
    family = Gamma(link = "log"))





