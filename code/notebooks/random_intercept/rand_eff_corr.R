library(tidyverse)
library(cmdstanr)
library(bayesplot)
library(posterior)

# Load data frames and lists
source(file.path("dataloader", "data_cleaning.R"))
rm(census, census_one_hot, mcs, mcs_stan)
source(file.path("ops", "binary_functions.R"))

base_model <- cmdstan_model(file.path("model", "stan", "rand_eff_spec", 
                                      "raneff_spec.model.stan"))

corr_model <- cmdstan_model(file.path("model", "stan", "rand_eff_spec", 
                                 "raneff_corr.model.stan"))

sar_model <- cmdstan_model(file.path("model", "stan", "rand_eff_spec", 
                                 "raneff_corr_sar.model.stan"))

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
                           iter_warmup = 1200, 
                           iter_sampling = 400,  
                           parallel_chains = 4,
                           seed = 123)

lkj_draws <- corr_model$sample(mcs_one_hot_strat, 
             chains = 4, 
             iter_warmup = 1200, 
             iter_sampling = 400,  
             parallel_chains = 4,
             seed = 123)

sar_draws <- sar_model$sample(mcs_one_hot_strat, 
             chains = 4, 
             iter_warmup = 1200, 
             iter_sampling = 400,  
             parallel_chains = 4,
             seed = 123)

sar_draws$draws("rho") %>% quantile(probs = seq(0, 1, 0.1))


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

loo_sar <- sar_draws$loo()

loo_comparison <- loo ::loo_compare(list(Base = loo_base, LKJ  = loo_lkj, SAR = loo_sar)) 

loo_comparison %>% xtable::xtable()

Omega_plot_df <- function(Omega, group){
  Omega %>%
    as_tibble() %>%
    rowid_to_column(var="X") %>%
    gather(key="Y", value="corr", -1) %>% 
    mutate(Y = as.numeric(str_sub(Y, start = 2))) %>% 
    mutate(corr = ifelse(X <= Y, NA, corr)) %>% 
    mutate(X_bin = map_chr(X - 1, to_binary), 
           Y_bin = map_chr(Y - 1, to_binary)) %>% 
    mutate(group = group)
}

Omega_grouped_df <- function(mcmc_draws, D, size = 4, invert = FALSE){
  Omega <- mcmc_draws$draws("Omega") %>% 
              as_draws_matrix
  num_iter <- nrow(Omega)
  idx <- sample(1:num_iter, size = size, replace = FALSE)
  result <- data.frame()
  
  for(i in seq_along(idx)){
    Omega_matrix <- matrix(Omega[idx[i],], ncol = D)
    if(invert){
      message("Inverting precision matrix")
      Omega_matrix <- cov2cor(solve(Omega_matrix))
      }
    result <- rbind(result, Omega_plot_df(Omega_matrix, i))
  }
  return(result)
}

Omega_plot_df_lkj <- Omega_grouped_df(lkj_draws, 32, 4)
Omega_plot_df_sar <- Omega_grouped_df(sar_draws, 32, 4, invert = TRUE)


corr_plot <- function(Omega, nrow = 2, axis_text = TRUE){ 
  graph <- ggplot(Omega, aes(X_bin, 
                   Y_bin, 
                   fill= corr)) + 
  geom_tile() + 
  theme_void() + 
  facet_wrap(vars(group), nrow = nrow) +
  scale_fill_gradient2(na.value = "#FFFFFF") +
  scale_x_discrete(limits = rev) 
  if(axis_text){
    graph <- graph +   
    theme(axis.text.x = element_text(angle = 90, size = 6),
                    axis.text.y = element_text(size = 6)) +
    theme(strip.text.x = element_blank())}
  return(graph)
  }

dens_corr_plot <- function(Omega) {
  ggplot(Omega, aes(corr, colour = factor(group))) + 
  geom_density(show.legend = FALSE) + 
  theme_minimal() + 
  # theme(axis.text.x = element_text(angle = 90, size = 8),
  #       axis.text.y = element_text(size = 8)) +
  theme(strip.text.x = element_blank(), text = element_text(size = 25)) +
  scale_color_discrete(name = "sample") +
  labs(x = "", y = "") 
  }

lkj_corr_plot <- corr_plot(Omega_plot_df_lkj)
sar_corr_plot <- corr_plot(Omega_plot_df_sar)
lkj_dens_plot <- dens_corr_plot(Omega_plot_df_lkj)
sar_dens_plot <- dens_corr_plot(Omega_plot_df_sar)

graph_path <- file.path("notebooks", "random_intercept", "graphs")

plot_save <- purrr::partial(ggsave, width = 16, height = 12, units = "cm")

plot_save(filename = str_c(graph_path, "/lkj_corr_plot.png"), 
          plot = lkj_corr_plot)
plot_save(filename = str_c(graph_path, "/sar_corr_plot.png"), 
          plot = sar_corr_plot)
plot_save(filename = str_c(graph_path, "/lkj_dens_plot.png"), 
          plot = lkj_dens_plot)
plot_save(filename = str_c(graph_path, "/sar_dens_plot.png"), 
          plot = sar_dens_plot)



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

data(oldcol, package = "spdep")
brms::make_stancode(CRIME ~ INC + HOVAL + sar(COL.nb, type = "error"), 
            data = COL.OLD, data2 = list(COL.nb = COL.nb),
            chains = 2, cores = 2)

