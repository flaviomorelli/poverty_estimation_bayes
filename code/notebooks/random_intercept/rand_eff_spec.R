library(tidyverse)
library(cmdstanr)
library(tidymodels)

source(file.path("dataloader", "data_cleaning.R"))
rm(census)
# Categorical variables: jsector, id_men, trabinusual, ingresoext, pobind, rururb

mcs_small <- mcs %>% 
  select(jsector, id_men, trabinusual, pcpering, ingresoext,
           actcom_pc, bienes_pc, pob_ind, rururb, mun,
           ic_rezedu, ic_asalud, ic_cv, ic_sbv, strat_idx) %>% 
  mutate(across(c("id_men", "trabinusual", "ingresoext", "pob_ind"), as.factor)) 

mcs_clean <- recipe( ~ ., mcs_small) %>% 
  step_dummy(all_of(c("jsector", "id_men", "trabinusual", 
                                 "ingresoext", "pob_ind", "rururb")), 
             one_hot = TRUE) %>% 
  prep() %>% juice()

strtoi(mcs$strat_idx, base = 2) + 1
