library(tidyverse)
library(brms)

source(file.path("dataloader", "data_cleaning.R"))

job::job(
  {log_fit_student <- brm(
    log(ictpc + 1) ~ jsector + jsexo + jexp + jedad +
      id_men + trabinusual + pcocup + pcpering + ingresoext +
      pcmuj + pcalfab + actcom_pc + bienes_pc + pob_ind + rururb + (1|mun), 
    data = mcs %>% filter(pcalfab <= 100, pcmuj <= 100), 
    family = student(), 
    prior = prior(normal(0, 0.5), class = "b"), 
    iter = 1800, 
    warmup = 1500, 
    chains = 4,
    control = list(adapt_delta = 0.9),
    cores = 4)
  })
# Not relevant: jexp, jedad, jsexo, pcocup, pcpering, pc_muj, pcalfab

pp_check(log_fit_student, type = "dens_overlay", nsamples = 50)

job::job(
  {log_fit_student_hs <- update(
    log_fit_student,
    family = student(), 
    prior = set_prior(horseshoe(df = 1, par_ratio = 0.2)), 
    iter = 1800, 
    warmup = 1500, 
    chains = 4,
    control = list(adapt_delta = 0.999, max_treedepth = 13),
    cores = 4)
})

mcmc_plot(log_fit_student_hs, type = "dens") %>% 
  ggsave(file.path("notebooks", 
                   "variable_selection", 
                   "student_hs_plot.png"), 
         plot = ., 
         height = 6, 
         width = 9)
# The following variables do not seem to be particularly relevant:
# jexp, jedad, jsexo, pcocup, (pcpering), pc_muj, pcalfab

mcmc_plot(log_fit_student_hs, type = "dens")

job::job(
  {log_fit_weights <-  update(log_fit_student, 
                           formula. = ~ . + factor,
                           newdata = mcs %>% filter(pcalfab <= 100, pcmuj <= 100), 
                           control = list(adapt_delta = 0.9),
                           cores = 4)
  })
mcmc_plot(log_fit_weights, type = "dens")

job::job(
  {log_fit_smallest <-  update(log_fit_student_hs, 
                              formula. = ~ . - jexp - jedad - jsexo - 
                                pcocup - pcpering - pcmuj - pcalfab,
                              control = list(adapt_delta = 0.999, max_treedepth = 13),
                              chains = 4,
                              cores = 4)
  })

job::job(
  {log_fit_smaller1_hs <-  update(log_fit_student_hs, 
                               formula. = ~ . - jexp - jsexo - jedad -
                                 pcocup - pcmuj - pcalfab,
                               control = list(adapt_delta = 0.99),
                               cores = 4)
  })

mcmc_plot(log_fit_smaller1_hs, type = "dens") %>% 
  ggsave(file.path("notebooks", 
                   "variable_selection", 
                   "student_hs_reduced_plot.png"), 
         plot = ., 
         height = 6, 
         width = 9)

job::job(
  {log_fit_smaller2_hs <-  update(log_fit_student_hs, 
                               formula. = ~ . - jsexo - 
                               pcmuj - pcalfab,
                               control = list(adapt_delta = 0.99),
                               cores = 4)
  })



pp_check(log_fit_student_hs, type = "loo_pit")
pp_check(log_fit_smaller1_hs, type = "loo_pit")
pp_check(log_fit_smaller2_hs, type = "loo_pit")

loo_compare(loo(log_fit_student_hs),
            loo(log_fit_smaller1_hs))

job::job(
  {log_fit_r2d2 <-  update(log_fit_student_hs, 
                            prior = set_prior(R2D2(mean_R2 = 0.7, prec_R2 = 10)),
                            control = list(adapt_delta = 0.9),
                            cores = 4)
  })
