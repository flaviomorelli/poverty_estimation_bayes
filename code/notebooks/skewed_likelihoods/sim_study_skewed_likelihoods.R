library(tidyverse)
library(brms)

# Load simulations
source(file.path("dataloader", "load_simulations.R"))

source(file.path("dataloader", "data_cleaning.R"))

bayesplot::bayesplot_theme_set(
    theme_minimal(base_size = 23) + 
        theme(legend.position = "none")
    )

reg_formula <- y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + (1|group_id)
scenarios <- c("logscale", "gb2", "pareto")
types <- c("smp", "smp_miss")

iteration_loop <- function(FUN, ...){
    result <- list()
    for(scenario in scenarios){
        message(str_c("Scenario: ", scenario))
        for(type in types){
            result[[scenario]][[type]] <- FUN(scenario, type, ...)
        }
    }
    return(result)
}

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

ppc <- function(scenario, type, models, distribution){
    path <- file.path("notebooks", 
                      "skewed_likelihoods",
                      "graphs",
                      distribution)
    
    graph <- pp_check(models[[scenario]][[type]], 
             type = "dens_overlay", nsamples = 100)
    
    ggsave(filename = str_c(path, "/",
                            scenario, "_", 
                            type, ".png"),
           plot = graph,
           width = 17, 
           height = 18, 
           units = "cm")
}

Q10 <- function(x) quantile(x, 0.1)
Q90 <- function(x) quantile(x, 0.9)

ppc_2d <- function(scenario, type, models, distribution){
    path <- file.path("notebooks", 
                      "skewed_likelihoods",
                      "graphs",
                      distribution)
    
    
    
    graph_1 <- pp_check(models[[scenario]][[type]], 
                        type = "stat_2d",
                        stat = c("mean", "sd"))
    
    graph_2 <- pp_check(models[[scenario]][[type]], 
                        type = "stat_2d",
                        stat = c("median", "IQR"))
    
    graph_3 <- pp_check(models[[scenario]][[type]], 
                        type = "stat_2d",
                        stat = c("Q10", "Q90"))
    
    graph <- bayesplot::bayesplot_grid(graph_1, 
                                       graph_2,
                                       graph_3,
                                       legends = FALSE)
    
    ggsave(filename = str_c(path, "/2d_",
                            scenario, "_", 
                            type, ".png"),
           plot = graph,
           width = 16, 
           height = 18, 
           units = "cm")
}


gamma_log_fit <- iteration_loop(brm_models, 
                                Gamma(link = "log"))
iteration_loop(ppc, gamma_log_fit, "gamma_log")
iteration_loop(ppc_2d, gamma_log_fit, "gamma_log")

# Some R-hat values slightly high, but no divergences
gamma_softplus_fit <- iteration_loop(brm_models, 
                                     brmsfamily("gamma", link = "softplus"))
iteration_loop(ppc, gamma_softplus_fit, "gamma_softplus")
iteration_loop(ppc_2d, gamma_softplus_fit, "gamma_softplus")

lognormal_fit <- iteration_loop(brm_models, 
                                lognormal())
iteration_loop(ppc, lognormal_fit, "lognormal")
iteration_loop(ppc_2d, lognormal_fit, "lognormal")

# Pareto smp with very high R-square. Does not capture distribution
skewnormal_fit <- iteration_loop(brm_models, 
                                 skew_normal())
iteration_loop(ppc, skewnormal_fit, "skew_normal")
iteration_loop(ppc_2d, skewnormal_fit, "skew_normal")

# Fitting problems. No model with small R-hat
exgaussian_fit <- 
    iteration_loop(brm_models, exgaussian(), 
     prior = prior(normal(0, 200), class = b),
     control = list(adapt_delta = 0.99),
     warmup = 3000, 
     iter = 3500)
iteration_loop(ppc, exgaussian_fit, "exgaussian")
iteration_loop(ppc_2d, exgaussian_fit, "exgaussian")

# Check Stan code for Gamma likelihood
make_stancode(reg_formula, 
    data = sim_data[["gb2"]][["smp"]],
    family = Gamma(link = "log"))

mcs_fit <- brm(
  ictpc_corr ~ jsexo  + pob_ind + jexp + jedad+
      actcom + bienes + pcpering + pcocup + jaesc + rururb + (1|mun), 
  data = mcs,
  family = exponential(),
  iter = 1300, 
  warmup = 1000,
  cores = 4)

pp_check(mcs_fit, type = "dens_overlay", nsamples = 100) + xlim(c(0, 25000))
pp_check(mcs_fit, type = "stat_2d", stat = c("median", "IQR"))
pp_check(mcs_fit, type = "stat_2d", stat = c("Q10", "Q90"))

make_stancode(
    ictpc_corr ~ jsexo  + pob_ind + jexp + jedad+
        actcom + bienes + pcpering + pcocup + jaesc + rururb + (1|mun), 
    prior = prior(normal(0, 0.35), class = b),
    data = mcs,
    family = Gamma(link = "log"))
