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

rw_model <- cmdstan_model(file.path("model", "stan", "rand_eff_spec", 
                                 "raneff_corr_rw.model.stan"))

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

lkj_draws$draws("u") %>% mcmc_dens()
rw_draws$draws("u") %>% mcmc_dens()

rw_draws <- rw_model$sample(mcs_one_hot_strat, 
             chains = 4, 
             iter_warmup = 1000, 
             iter_sampling = 250,  
             parallel_chains = 4,
             adapt_delta = 0.9,
             tree_depth = 13
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

loo_rw <- rw_draws$loo()

loo_slope <- slope_draws$loo()

loo_compare(loo_base, loo_lkj, loo_rw)

to_binary <- function(x) str_sub(paste(rev(as.integer(intToBits(x))), collapse=""), start = -5)
Omega_plot_df <- function(Omega, group){
  Omega %>%
    as_tibble() %>%
    rowid_to_column(var="X") %>%
    gather(key="Y", value="corr", -1) %>% 
    mutate(Y = 33 - as.numeric(str_sub(Y, start = 2))) %>% 
    mutate(corr = ifelse(X >= 33 - Y, NA, corr)) %>% 
    mutate(X_bin = map_chr(X - 1, to_binary), 
           Y_bin = map_chr(Y - 1, to_binary)) %>% 
    mutate(group = group)
}

Omega_grouped_df <- function(mcmc_draws, D, size = 4){
  Omega <- mcmc_draws$draws("Omega") %>% 
              as_draws_matrix
  num_iter <- nrow(Omega)
  idx <- sample(1:num_iter, size = size, replace = FALSE)
  result <- data.frame()
  
  for(i in seq_along(idx)){
    Omega_matrix <- Omega[idx[i],] %>% matrix(ncol = D)
    result <- rbind(result, Omega_plot_df(Omega_matrix, i))
  }
  return(result)
}


Omega_plot_grouped <- Omega_grouped_df(lkj_draws, 32, 4)

corr_plot <- ggplot(Omega_plot_grouped, aes(X_bin, 
                               Y_bin, 
                               fill= corr)) + 
  geom_tile() + 
  theme_void() + 
  theme(axis.text.x = element_text(angle = 90, size = 4),
        axis.text.y = element_text(size = 4)) +
  theme(strip.text.x = element_blank()) + 
  facet_wrap(vars(group), nrow = 2) +
  scale_fill_gradient2(na.value = "#FFFFFF")

dens_corr_plot <- ggplot(Omega_plot_grouped, aes(corr, colour = factor(group))) + 
  geom_density() + 
  theme_minimal() + 
  # theme(axis.text.x = element_text(angle = 90, size = 8),
  #       axis.text.y = element_text(size = 8)) +
  theme(strip.text.x = element_blank())

graph_path <- file.path("notebooks", "random_intercept", "graphs")

ggsave(str_c(graph_path, "/corr_plot.png"), plot = corr_plot,
      width = 10, height = 8, units = "cm")
ggsave(str_c(graph_path, "/dens_corr_plot.png"), plot = dens_corr_plot,
       width = 9, height = 8, units = "cm")

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

ident <- read.csv2("/Users/flaviomejia/Downloads/Identificacion ( Capitulo A)/Identificacion ( Capitulo A).csv") %>% tibble
viv_hog <- read.csv2("/Users/flaviomejia/Downloads/Viviendas y hogares/Viviendas y hogares.csv") %>% tibble
datos_viv<- read.csv2("/Users/flaviomejia/Downloads/Datos de la vivienda/Datos de la vivienda.csv") %>% tibble
