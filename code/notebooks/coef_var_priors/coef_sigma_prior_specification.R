library(tidyverse)
library(cmdstanr)
library(bayesplot)
library(posterior)

# Data loader for MCS and simulations
source(file.path("dataloader", "data_cleaning.R"))
source(file.path("dataloader", "load_simulations.R"))
rm(census)

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
              alpha = 0.2, size = 2) +
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
         plot = graph, width = 9, height = 8, units = "cm")
    )
}

X <- mcs %>% select(jsector, jsexo, jexp, jedad,
                      id_men, trabinusual, pcocup, pcpering, ingresoext,
                      pcmuj, pcalfab, actcom_pc, bienes_pc, pob_ind, rururb)

domain <- sapply(mcs$mun, function(x) which(unique(mcs$mun) == x))
stan_data <- list(N = nrow(mcs),
                  K = ncol(X), 
                  D = length(unique(mcs$mun)), 
                  y = mcs$ictpc, 
                  X = X, 
                  domain = domain)
sim_data$logscale$


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
