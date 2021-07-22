library(tidyverse)
library(cmdstanr)
library(bayesplot)
library(posterior)

source(file.path("dataloader", "data_cleaning.R"))
source(file.path("dataloader", "load_simulations.R"))
rm(census)

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


model <- cmdstan_model(
    file.path(
      "model", 
      "stan", 
      "prior_predictive_checks",
      "coefficient_specification.stan"
      )
    )

mcmc_draws <- model$sample(
          data = sim_data$gb2$smp_stan,
          chains = 2,
          fixed_param = TRUE
          )

y <- sim_data$gb2$smp_stan$y
y_pred <- mcmc_draws$draws("y_pred") %>% as_draws_matrix
sum(y_pred == Inf) / length(y_pred)


ppc_scatter(log(y), log(y_pred[1:16,]), 
            alpha = 0.4, size = 2) +
  #xlim(c(-100, 50000)) + 
  #ylim(c(0, 0.005)) + 
  theme_minimal()

ppc_dens_overlay(log(y), 
                 log(y_pred[1:50,]), freq = FALSE) +
  #xlim(c(-100, 100)) + 
  #ylim(c(0, 0.005)) + 
  theme_minimal()

min(y_pred)
quantile(y_pred)
sum(y_pred < 0)

mcmc_dens(mcmc_draws$draws(), pars = "u[1]")





       