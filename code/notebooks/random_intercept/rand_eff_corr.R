library(tidyverse)
library(cmdstanr)
library(bayesplot)
library(posterior)
library(loo)

# Load data frames and lists
source(file.path("dataloader", "data_cleaning.R"))
rm(census, census_one_hot, mcs, mcs_stan)

base_model <- cmdstan_model(file.path("model", "stan", "rand_eff_spec", 
                                      "raneff_spec.model.stan"))

corr_model <- cmdstan_model(file.path("model", "stan", "rand_eff_spec", 
                                 "raneff_corr.model.stan"))

slope_model <- cmdstan_model(file.path("model", "stan", "rand_eff_spec", 
                                      "raneff_corr_random_slope.model.stan"))

base_draws <- base_model$sample(mcs_one_hot_strat, 
                           chains = 4, 
                           iter_warmup = 1000, 
                           iter_sampling = 250,  
                           parallel_chains = 4,
                           adapt_delta = 0.9)

lkj_draws <- corr_model$sample(mcs_one_hot_strat, 
             chains = 4, 
             iter_warmup = 1000, 
             iter_sampling = 250,  
             parallel_chains = 4,
             adapt_delta = 0.9)

slope_draws <- slope_model$sample(mcs_one_hot_strat, 
                               chains = 4, 
                               iter_warmup = 1000, 
                               iter_sampling = 250,  
                               parallel_chains = 4,
                               adapt_delta = 0.9)

loo_base <- base_draws$loo()
#           Estimate    SE
# elpd_loo -14855.8  50.7
# p_loo        30.6   0.8
# looic     29711.7 101.3

loo_lkj <- lkj_draws$loo()
#         Estimate    SE
# elpd_loo -14855.3  50.7
# p_loo        30.0   0.8
# looic     29710.5 101.4

loo_slope <- slope_draws$loo()
#          Estimate    SE
# elpd_loo -14862.4  54.6
# p_loo       171.9   6.3
# looic     29724.8 109.2

# Random slopes don't seem to be particularly useful
loo_compare(loo_base, loo_lkj, loo_slope)
#          elpd_diff se_diff
# model2  0.0       0.0   
# model1 -0.6       0.2   
# model3 -7.1      14.0  

# Variance, nor correlation!
lkj_Sigma_draw <- lkj_draws$draws("Sigma") %>% as_draws_df %>% .[1,1:1024] %>% unlist 

Sigma <- matrix(lkj_Sigma_draw, ncol = 32) 

Sigma_plot <- Sigma %>%
  as_tibble() %>%
  rowid_to_column(var="X") %>%
  gather(key="Y", value="Z", -1) %>% 
  mutate(Y = 33 - as.numeric(str_sub(Y, start = 2)))

ggplot(Sigma_plot, aes(X, Y, fill= Z)) + 
  geom_tile() 

heatmap(Sigma, Rowv = FALSE, Colv = FALSE, hclustfun = NULL)

