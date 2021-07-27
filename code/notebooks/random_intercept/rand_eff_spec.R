library(tidyverse)
library(tidymodels)
library(cmdstanr)
library(bayesplot)
library(posterior)
library(brms)

# Load data frames and lists
source(file.path("dataloader", "data_cleaning.R"))
rm(census, census_one_hot, mcs, mcs_stan)

model <- cmdstan_model(file.path("model", "stan", "rand_eff_spec", 
                                 "raneff_spec.model.stan"))

model$variational(mcs_one_hot_strat)

mun_draws <- model$sample(mcs_one_hot_mun, 
                          chains = 4, 
                          iter_warmup = 400,
                          iter_sampling = 200,
                          parallel_chains = 4,
                          max_treedepth = 13, 
                          adapt_delta = 0.99)

strat_draws <- model$sample(mcs_one_hot_strat, 
                          chains = 4, 
                          iter_warmup = 600, 
                          iter_sampling = 200,
                          parallel_chains = 4,
                          max_treedepth = 14, 
                          adapt_delta = 0.95)


mun_draws$draws("sigma") %>% mcmc_rank_hist()

make_stancode(log(ictpc + 1) ~ pcpering + actcom_pc + bienes_pc + 
      jsector_Primario + jsector_Secundario + jsector_Terciario + id_men_TRUE. + (1|mun), 
     data = mcs_one_hot, 
     family = student(), 
     cores = 4)
