library(brms)
library(tidyverse)

# Load census and mcs data sets
source("dataloader/data_cleaning.R")
source("ops/trafo.R")

shift <- find_shift(mcs$ictpc)$minimizer %>% round

# Count number of observations that are exactly 0
sum(mcs$ictpc == 0)

base_model  <- brm(
           log(ictpc + 81) ~ jsector + pob_ind + discap_hog + rururb +
                    + id_men + jsexo + trabinusual + ingresoext +
                    scale(actcom) + scale(bienes) + scale(jexp) + 
                    scale(pcocup) + scale(pcpering) + scale(jedad) +
                    scale(pcmuj) + scale(pcalfab) + (1|mun), 
           data = mcs, 
           save_pars = save_pars(all = TRUE), 
           backend = "rstan",
           chains = 2, 
           cores = 2,
           control = list(adapt_delta = 0.98)
          )

student_model <- update(base_model, 
                        family = student(),
                        cores = 2
                        )

student_model_compact <- update(base_model,
                        formula. = ~ . -scale(jedad) -
                          scale(pcmuj) - scale(pcalfab) - 
                          jsexo - id_men - pob_ind,
                        family = student(),
                        cores = 2
                        )

skew_normal_model <- brm(ictpc_corr ~ jsector + pob_ind + discap_hog + rururb +
                         + id_men + jsexo + trabinusual + ingresoext +
                         scale(actcom) + scale(bienes) + scale(jexp) + 
                         scale(pcocup) + scale(pcpering) + scale(jedad) +
                         scale(pcmuj) + scale(pcalfab) + (1|mun), 
                        data = mcs,
                        family = skew_normal(),
                        save_pars = save_pars(all = TRUE), 
                        backend = "rstan",
                        cores = 2,
                        chains = 2
                        )

lognormal_model <- update(skew_normal_model, 
                       data = mcs,
                       family = lognormal(),
                       cores = 2,
                       chains = 2
                        )

gamma_model <- update(skew_normal_model, 
                       data = mcs,
                       family = Gamma(link="log"),
                       cores = 2,
                       chains = 2
                      )


horseshoe_model <- update(base_model, 
                          prior = set_prior(horseshoe(df = 3, 
                                                      par_ratio = 0.4)),
                          cores = 2
                          )

horseshoe_student_50 <- update(student_model, 
                            prior = set_prior(horseshoe(df = 3, 
                                                        par_ratio = 0.5)),
                            cores = 2
                            )

horseshoe_student_30 <- update(student_model, 
                               prior = set_prior(horseshoe(df = 3, 
                                                           par_ratio = 0.3)),
                               cores = 2
                              )

horseshoe_student_10 <- update(student_model, 
                               prior = set_prior(horseshoe(df = 3, 
                                                           par_ratio = 0.1)),
                               cores = 2
                                )

horseshoe_student_05 <- update(student_model, 
                               prior = set_prior(horseshoe(df = 3, 
                                                           par_ratio = 0.05)),
                               cores = 2
                              )


loo_base <- loo(base_model, moment_match = TRUE)
loo_student <- loo(student_model, moment_match = TRUE)
loo_student_compact <- loo(student_model_compact)
loo_hs <- loo(horseshoe_model)
loo_hs_student_50 <- loo(horseshoe_student_50)
loo_hs_student_30 <- loo(horseshoe_student_30)
loo_hs_student_10 <- loo(horseshoe_student_10)
loo_hs_student_05 <- loo(horseshoe_student_05)


loo_compare(loo_base, loo_student, 
            loo_student_compact,
            loo_hs_student_50, 
            loo_hs_student_30,
            loo_hs_student_10,
            loo_hs_student_05)

# Visual checks
mcmc_plot(horseshoe_student)
mcmc_plot(horseshoe_student, type = "hist")
mcmc_plot(horseshoe_student, type = "nuts_acceptance")
mcmc_plot(horseshoe_student, type = "nuts_divergence")

pp_check(gamma_model, nsamples = 30)
pp_average(base_model, student_model)

# Quick test of the model with the alternative 
stratified_model  <- brm(
  log(ictpc + 81) ~ jsector + pob_ind + discap_hog + rururb +
    + id_men + jsexo + trabinusual + ingresoext +
    scale(actcom) + scale(bienes) + scale(jexp) + 
    scale(pcocup) + scale(pcpering) + scale(jedad) +
    scale(pcmuj) + scale(pcalfab) +
    (1|ic_cv:ic_sbv:ic_rezedu:ic_asalud:rururb), 
  data = mcs, 
  family = student(),
  save_pars = save_pars(all = TRUE), 
  backend = "rstan",
  chains = 2, 
  cores = 2,
  control = list(adapt_delta = 0.95)
)