library(tidyverse)
library(cmdstanr)
library(bayesplot)
library(posterior)
library(brms)

# Data loader for simulations
source(file.path("dataloader", "load_simulations.R"))

# Import stan_helper functions
source(file.path("ops", "stan_helper.R")) # includes iteration_loop


prior_samples <- function(scenario, type, data, model){ 
  model$sample(data[[scenario]][[str_c(type, "_stan")]], 
               fixed_param = TRUE)
  }

prior_scatter <- function(scenario, type, data, samples){
  y <- sim_data[[scenario]][[str_c(type, "_stan")]][["y"]]
  y_pred <- as_draws_matrix(samples[[scenario]][[type]]$draws("y_pred"))
  return(ppc_scatter(log(y), 
              log(y_pred[1:16, ]), 
              alpha = 0.2, size = 1) +
    theme_minimal()
    )
}

prior_scatter_save <- function(scenario, type, graphs, name){ 
  graph <- graphs[[scenario]][[type]]
  return(
    ggsave(file.path("notebooks", 
                   "coef_var_priors", 
                   "graphs", 
                   str_c("prior_check_", scenario, "_", name, ".png")),
         plot = graph, width = 15, height = 13, units = "cm")
    )
}

start_model <- cmdstan_model(
    file.path(
      "model", 
      "stan", 
      "prior_predictive_checks",
      "coeff_var_specification_start.model.stan"
      )
    )

start_draws <- iteration_loop(FUN = prior_samples,
                              types = c("smp"), 
                              data = sim_data,
                              model = start_model) 

start_prior_check <- iteration_loop(FUN = prior_scatter,
                              types = c("smp"), 
                              data = sim_data,
                              samples = start_draws) 

iteration_loop(FUN = prior_scatter_save,
               types = c("smp"), 
               graphs = start_prior_check,
               name = "start")

tight_model <- cmdstan_model(
  file.path(
    "model", 
    "stan", 
    "prior_predictive_checks",
    "coeff_var_specification_tighter.model.stan"
  )
) 

tight_draws <- iteration_loop(FUN = prior_samples,
                              types = c("smp"), 
                              data = sim_data,
                              model = tight_model) 

tight_prior_check <- iteration_loop(FUN = prior_scatter,
                                    types = c("smp"), 
                                    data = sim_data,
                                    samples = tight_draws) 

iteration_loop(FUN = prior_scatter_save,
               types = c("smp"), 
               graphs = tight_prior_check,
               name = "tight")

wide_model <- cmdstan_model(
  file.path(
    "model", 
    "stan", 
    "prior_predictive_checks",
    "coeff_var_specification_wider.model.stan"
  )
) 

wide_draws <- iteration_loop(FUN = prior_samples,
                              types = c("smp"), 
                              data = sim_data,
                              model = wide_model) 

wide_prior_check <- iteration_loop(FUN = prior_scatter,
                                    types = c("smp"), 
                                    data = sim_data,
                                    samples = wide_draws) 

iteration_loop(FUN = prior_scatter_save,
               types = c("smp"), 
               graphs = wide_prior_check,
               name = "wide")

horseshoe_pred <- brm(log(y) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + (1|group_id), 
    data = sim_data$pareto$smp, 
    sample_prior = "only",
    family = student(), 
    prior = prior(horseshoe()) +  
              prior_string("gamma(2, 10)", class = "sd") + 
              prior_string("gamma(2, 1)", class = "nu"), 
    chains = 1
    ) 


pp_check(horseshoe_pred, type = "hist", nsamples = 15) + xlim(c(5, 15))
