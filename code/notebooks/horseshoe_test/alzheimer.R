library(tidymodels)
library(tidyverse)
library(glmnet)
library(cmdstanr)
library(bayesplot)


data("ad_data")
ad_data
View(ad_data)

regressors <- ad_data %>% select(-Class, -Genotype, -Cystatin_C, -male)
regressors <- regressors %>% apply(MARGIN = 2, scale)
y <- ad_data$Cystatin_C %>% scale %>% as.numeric

lambda <- cv.glmnet(regressors %>% as.matrix, y)$lambda.1se
glmnet(regressors %>% as.matrix, y, lambda = lambda) %>% coef

stan_data <- list(X = regressors, 
                  y = y, 
                  N = nrow(regressors), 
                  K = ncol(regressors))

model <- cmdstan_model("notebooks/horseshoe_test/regularized_horseshoe.stan")
vb_fit <- model$variational(stan_data)
nuts_fit <- model$sample(data = stan_data, 
                         seed = 1234, 
                         chains = 2, 
                         parallel_chains = 2, 
                         adapt_delta = 0.95, 
                         max_treedepth = 12,
                         refresh = 10, 
                         iter_warmup = 500,
                         iter_sampling = 300)

nuts_fit$cmdstan_diagnose()
nuts_fit$cmdstan_summary()
nuts_fit$cmdstan_diagnose()
nuts_fit$summary("beta")

posterior::as_draws(nuts_fit$draws()) 
mcmc_hist(nuts_fit$draws("lambda_0"))

X_idx <- vb_fit$summary("beta") %>% 
  filter(sign(q5) == sign(q95), 
         abs(median) > 0.5) %>% 
  select(variable) %>% 
  mutate(variable = str_sub(variable, 6, 7) %>% as.numeric) %>% 
  unlist

regressors[, X_idx] %>% colnames
