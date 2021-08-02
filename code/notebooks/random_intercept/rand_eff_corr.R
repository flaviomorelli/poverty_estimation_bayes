library(tidyverse)
library(cmdstanr)
library(bayesplot)
library(posterior)

# Load data frames and lists
source(file.path("dataloader", "data_cleaning.R"))
rm(census, census_one_hot, mcs, mcs_stan)

base_model <- cmdstan_model(file.path("model", "stan", "rand_eff_spec", 
                                      "raneff_spec.model.stan"))

corr_model <- cmdstan_model(file.path("model", "stan", "rand_eff_spec", 
                                 "raneff_corr.model.stan"))

slope_model <- cmdstan_model(file.path("model", "stan", "rand_eff_spec", 
                                      "raneff_corr_random_slope.model.stan"))

slope_rstan <- rstan::stan(file.path("model", "stan", "rand_eff_spec", 
                      "raneff_corr_random_slope.model.stan"), 
            chains = 4, 
            iter = 1250, 
            warmup = 1000, 
            control = list(adapt_delta = 0.9))

base_draws <- base_model$sample(mcs_one_hot_strat, 
                           chains = 4, 
                           iter_warmup = 1000, 
                           iter_sampling = 250,  
                           parallel_chains = 4,
                           adapt_delta = 0.9, 
                           seed = 123)

lkj_draws <- corr_model$sample(mcs_one_hot_strat, 
             chains = 4, 
             iter_warmup = 1000, 
             iter_sampling = 250,  
             parallel_chains = 4,
             adapt_delta = 0.9, 
             seed = 123)

slope_draws <- slope_model$sample(mcs_one_hot_strat, 
                               chains = 4, 
                               iter_warmup = 1000, 
                               iter_sampling = 250,  
                               parallel_chains = 4,
                               adapt_delta = 0.9, 
                               seed = 123)

loo_base <- base_draws$loo()

loo_lkj <- lkj_draws$loo()

loo_slope <- slope_draws$loo()

loo_compare(loo_base, loo_lkj)

lkj_Omega_draw <- lkj_draws$draws("Omega") %>% as_draws_df %>% .[1,1:1024] %>% unlist 

Omega <- matrix(lkj_Omega_draw, ncol = 32) 

Omega_plot <- Omega %>%
  as_tibble() %>%
  rowid_to_column(var="X") %>%
  gather(key="Y", value="Z", -1) %>% 
  mutate(Y = 33 - as.numeric(str_sub(Y, start = 2))) %>% 
  mutate(Z = ifelse(X >= 33 - Y, NA, Z))

ggplot(Omega_plot, aes(X, Y, fill= Z)) + 
  geom_tile() + theme_void() + 
  scale_fill_gradient2(na.value = "#FFFFFF")

# LKJ prior tests
# Create corr. matrix

corr_matrix <- function(rho = 0.2, K = 3){
  m <- matrix(runif(K^2, rho/2, rho), nrow = 3)
  diag(m) <- 1
  return(m)
}

corr_high <- corr_matrix(0.9)
corr_low <- corr_matrix(0.01)

# Higher correlation implies a lower determinant
det(corr_high)^10
det(corr_low)^10


