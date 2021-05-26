library(brms)
library(tidyverse)

# Load census and mcs data sets
source("dataloader/data_cleaning.R")
source("ops/trafo.R")

shift <- find_shift(mcs$ictpc)

# Count number of observations that are exactly 0
sum(mcs$ictpc == 0)

base_model  <- brm(
           log(ictpc + 81) ~ jsector + pob_ind + discap_hog +
                    scale(actcom) + scale(bienes) + scale(jexp) + 
                    scale(pcocup) + scale(pcpering) +(1|mun), 
           data = mcs, 
           save_pars = save_pars(all = TRUE), 
           backend = "rstan",
           chains = 2, 
           cores = 2,
           control = list(adapt_delta = 0.95)
          )

student_model <- update(base_model, 
                        family = student(),
                        cores = 2
                        )

skew_normal_model <- brm(ictpc ~ jsector + pob_ind + discap_hog +
                              scale(actcom) + scale(bienes) + scale(jexp) + 
                              scale(pcocup) + scale(pcpering) +(1|mun), 
                        data = mcs,
                        family = skew_normal(),
                        cores = 2,
                        chains = 2
                        )

lognormal_model <- brm(ictpc_corr ~ jsector + pob_ind + discap_hog +
                         scale(actcom) + scale(bienes) + scale(jexp) + 
                         scale(pcocup) + scale(pcpering) +(1|mun), 
                       data = mcs,
                       family = lognormal(),
                       cores = 2,
                       chains = 2
                        )

gamma_model <- brm(ictpc_corr ~ jsector + pob_ind + discap_hog +
                         scale(actcom) + scale(bienes) + scale(jexp) + 
                         scale(pcocup) + scale(pcpering) +(1|mun), 
                       data = mcs,
                       family = Gamma(link="log"),
                       cores = 2,
                       chains = 2
                      )



horseshoe_model <- update(base_model, 
                          prior = set_prior(horseshoe(df = 3, 
                                                      par_ratio = 0.5)),
                          cores = 2
                          )

horseshoe_student <- update(student_model, 
                            prior = set_prior(horseshoe(df = 3, 
                                                        par_ratio = 0.5)),
                            cores = 2
                            )

stratified_model  <- brm(
                  log(ictpc + 81) ~ jsector + pob_ind + discap_hog +
                    scale(actcom) + scale(bienes) + scale(jexp) + 
                    scale(pcocup) + scale(pcpering) + 
                    (1|ic_cv:ic_sbv:ic_rezedu:ic_asalud:rururb), 
                  data = mcs, 
                  family = student(),
                  save_pars = save_pars(all = TRUE), 
                  backend = "rstan",
                  chains = 2, 
                  cores = 2,
                  control = list(adapt_delta = 0.95)
                )

mcmc_plot(horseshoe_student)
mcmc_plot(horseshoe_student, type = "hist")
mcmc_plot(horseshoe_student, type = "nuts_acceptance")
mcmc_plot(horseshoe_student, type = "nuts_divergence")

pp_check(gamma_model, nsamples = 30)
pp_average(base_model, student_model)

loo_base <- loo(base_model, moment_match = TRUE)
loo_student <- loo(student_model, moment_match = TRUE)
loo_hs <- loo(horseshoe_model)
loo_hs_student <- loo(horseshoe_student)

varsel(horseshoe_model)



loo_compare(loo_base, 
            loo_student, 
            loo_hs, 
            loo_hs_student)
