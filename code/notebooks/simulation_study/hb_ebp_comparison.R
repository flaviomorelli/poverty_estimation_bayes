library(tidyverse)
library(cmdstanr)

# Load configuration (seed, paths)
source(file.path("config", "sim_config.R"))

# Load simulations
source(file.path("dataloader", "load_simulations.R"))

# Load numeric functions
source(file.path("ops", "indicators.R"))

# Load helper functions for Stan
source(file.path("ops", "stan_helper.R"))

# Load graph functionality
source(file.path("utils", "graphics", "sim_graphs.R"))

# Setup
scenarios <- list(logscale = "logscale", gb2 = "gb2", pareto = "pareto")

log_shift_model <- cmdstan_model(model_path)
end_model_no_corr <- cmdstan_model(file.path("model", "stan", 
                                           "rand_eff_spec", 
                                           "raneff_spec_base.model.stan"))
gq_model <- cmdstan_model(gq_path)
gq_miss_model <- cmdstan_model(gq_miss_path)

end_gq_model <- cmdstan_model(file.path("model", "stan", 
                                        "rand_eff_spec", 
                                        "raneff_spec_y_pred.model.stan"))
end_gq_miss_model <- cmdstan_model(file.path("model", "stan", 
                                        "rand_eff_spec", 
                                        "raneff_spec_y_pred_miss.model.stan"))

# Fit models and get predictions
fit <- lapply(scenarios,
              scenario_fit, 
              data = sim_data, 
              model = log_shift_model, 
              chains = 4, 
              iter_warmup = 1200, 
              iter_sampling = 300)
                            
y_pred <- scenario_pred_list(sim_data, 
                             gq_model,
                             gq_miss_model,
                             fit, 
                             scenarios)

end_fit <- lapply(scenarios,
                  scenario_fit, 
                  data = sim_data, 
                  model = end_model_no_corr, 
                  chains = 4, 
                  iter_warmup = 1200, 
                  iter_sampling = 300)

y_pred_end <- scenario_pred_list(sim_data, 
                             end_gq_model,
                             end_gq_miss_model,
                             end_fit, 
                             scenarios, 
                             regressors = "pop")

create_graphs(sim_data, y_pred, scenarios, graph_path = graph_path)
skewness_graphs(fit, scenarios)

# Comparison HB / EBP
hcr_pop <- indicator_pop_list(sim_data, scenarios, "hcr")
pgap_pop <- indicator_pop_list(sim_data, scenarios, "pgap")

hcr_sample <- indicator_list(sim_data, y_pred_end, scenarios, "hcr")
pgap_sample <- indicator_list(sim_data, y_pred_end, scenarios, "pgap")

hcr_hb <- hb_list(hcr_sample, scenarios)
pgap_hb <- hb_list(pgap_sample, scenarios)

# create_graphs(hcr_pop, hcr_sample, scenarios,  "hcr")
# create_graphs(pgap_pop, pgap_sample, scenarios, "pgap")

hcr_diagnostics <- diagnostics_list(hcr_sample, hcr_pop, scenarios)
pgap_diagnostics <- diagnostics_list(pgap_sample, pgap_pop, scenarios)

# EBP  

ebp_indicators <- ebp_indicators(sim_data, scenarios) 

compare_diagnostics(ebp_indicators, 
                    hcr_diagnostics, 
                    scenarios)

indicator_kde_graph(hcr_pop$pareto$pop,
                    ebp_indicators$pareto$smp$ind$Head_Count,
                    hcr_hb$pareto$smp) 

hb_ebp_scatter <- function(hb_value, ebp_value){
        ggplot(data.frame(HB = hb_value, EBP = ebp_value)) +
                theme_minimal() +
                geom_point(mapping = aes(HB, EBP), color = "#777777", alpha = 0.8) +
                geom_abline(intercept = 0, slope = 1, color = "gray") + 
                theme(text = element_text(size = 13)) +
                scale_x_continuous(breaks = scales::breaks_pretty(n = 3))
}

save_plot <- purrr::partial(ggsave, width = 7, height = 6, unit = "cm")
hb_ind_path <- file.path("notebooks", "simulation_study", "hb_ebp_comparison")

# RMSE comparison
hcr_logscale_rmse_comp <- hb_ebp_scatter(hcr_diagnostics$logscale$smp$rmse, 
                                          sqrt(ebp_indicators$logscale$smp$MSE$Head_Count))
save_plot(filename = str_c(hb_ind_path, "/hcr_logscale_rmse.png"), plot = hcr_logscale_rmse_comp)

hcr_gb2_rmse_comp <- hb_ebp_scatter(hcr_diagnostics$gb2$smp$rmse, 
                                              sqrt(ebp_indicators$gb2$smp$MSE$Head_Count))
save_plot(filename = str_c(hb_ind_path, "/hcr_gb2_rmse.png"), plot = hcr_gb2_rmse_comp)

hcr_pareto_rmse_comp <- hb_ebp_scatter(hcr_diagnostics$pareto$smp$rmse, 
                                         sqrt(ebp_indicators$pareto$smp$MSE$Head_Count))
save_plot(filename = str_c(hb_ind_path, "/hcr_pareto_rmse.png"), plot = hcr_pareto_rmse_comp)

pgap_logscale_rmse_comp <- hb_ebp_scatter(pgap_diagnostics$logscale$smp$rmse, 
                                              sqrt(ebp_indicators$logscale$smp$MSE$Poverty_Gap))
save_plot(filename = str_c(hb_ind_path, "/pgap_logscale_rmse.png"), plot = pgap_logscale_rmse_comp)

pgap_gb2_rmse_comp <- hb_ebp_scatter(pgap_diagnostics$gb2$smp$rmse, 
                                         sqrt(ebp_indicators$gb2$smp$MSE$Poverty_Gap))
save_plot(filename = str_c(hb_ind_path, "/pgap_gb2_rmse.png"), plot = pgap_gb2_rmse_comp)

pgap_pareto_rmse_comp <- hb_ebp_scatter(pgap_diagnostics$pareto$smp$rmse, 
                                            sqrt(ebp_indicators$pareto$smp$MSE$Poverty_Gap))
save_plot(filename = str_c(hb_ind_path, "/pgap_pareto_rmse.png"), plot = pgap_pareto_rmse_comp)

# Indicator comparison
hcr_logscale_ind_comp <- hb_ebp_scatter(hcr_hb$logscale$smp, 
                                              ebp_indicators$logscale$smp$ind$Head_Count)
save_plot(filename = str_c(hb_ind_path, "/hcr_logscale_ind.png"), plot = hcr_logscale_ind_comp)

hcr_gb2_ind_comp <- hb_ebp_scatter(hcr_hb$gb2$smp, 
                                         ebp_indicators$gb2$smp$ind$Head_Count)
save_plot(filename = str_c(hb_ind_path, "/hcr_gb2_ind.png"), plot = hcr_gb2_ind_comp)

hcr_pareto_ind_comp <- hb_ebp_scatter(hcr_hb$pareto$smp, 
                                            ebp_indicators$pareto$smp$ind$Head_Count)
save_plot(filename = str_c(hb_ind_path, "/hcr_pareto_ind.png"), plot = hcr_pareto_ind_comp)

pgap_logscale_ind_comp <- hb_ebp_scatter(pgap_hb$logscale$smp, 
                                               ebp_indicators$logscale$smp$ind$Poverty_Gap)
save_plot(filename = str_c(hb_ind_path, "/pgap_logscale_ind.png"), plot = pgap_logscale_ind_comp)

pgap_gb2_ind_comp <- hb_ebp_scatter(pgap_hb$gb2$smp, 
                                          ebp_indicators$gb2$smp$ind$Poverty_Gap)
save_plot(filename = str_c(hb_ind_path, "/pgap_gb2_ind.png"), plot = pgap_gb2_ind_comp)

pgap_pareto_ind_comp <- hb_ebp_scatter(pgap_hb$pareto$smp, 
                                             ebp_indicators$pareto$smp$ind$Poverty_Gap)
save_plot(filename = str_c(hb_ind_path, "/pgap_pareto_ind.png"), plot = pgap_pareto_ind_comp)


# Create skewness plots

bad_model <- cmdstan_model(file.path("model", "stan", "simulations", 
                                     "log_shift_wide_skewness.model.stan"))

bad_fit_logscale <- bad_model$sample(sim_data$logscale$smp_stan, 
                                parallel_chains = 4,
                                iter_warmup = 1200,
                                iter_sampling = 300)

bad_fit_gb2 <- bad_model$sample(sim_data$gb2$smp_stan, 
                            parallel_chains = 4,
                            iter_warmup = 1200,
                            iter_sampling = 300)

bad_fit_pareto <- bad_model$sample(sim_data$pareto$smp_stan, 
                                parallel_chains = 4,
                                iter_warmup = 1200,
                                iter_sampling = 300)

bad_fit_skewness <- bad_fit_gb2$draws() %>% mcmc_dens(pars = "s") + xlab("")

tight_model <- cmdstan_model(file.path("model", "stan", "simulations", 
                                     "log_shift_tight_skewness.model.stan"))
tight_fit_logscale <- tight_model$sample(sim_data$logscale$smp_stan, 
                                      parallel_chains = 4,
                                      iter_warmup = 1200,
                                      iter_sampling = 300)

tight_fit_gb2 <- tight_model$sample(sim_data$gb2$smp_stan, 
                            parallel_chains = 4,
                            iter_warmup = 1200,
                            iter_sampling = 300)

tight_fit_pareto <- tight_model$sample(sim_data$pareto$smp_stan, 
                                      parallel_chains = 4,
                                      iter_warmup = 1200,
                                      iter_sampling = 300)

tight_skewness <- tight_fit_gb2$draws() %>% mcmc_dens(pars = "s") + xlab("")

ggsave(file.path("notebooks", "simulation_study", "bad_skewness.png"), 
         plot = bad_fit_skewness, 
         width = 8,
         height = 7, 
         units = "cm")

ggsave(file.path("notebooks", "simulation_study", "tight_skewness.png"), 
       plot = tight_skewness, 
       width = 8,
       height = 7, 
       units = "cm")

gb2_skewness <- fit$gb2$smp$draws() %>% mcmc_dens(pars = "s") + xlab("")
ggsave(file.path("notebooks", "simulation_study", "gb2_skewness.png"), 
       plot = gb2_skewness, 
       width = 8,
       height = 7, 
       units = "cm")

loo::loo_compare(tight_fit_logscale$loo(), 
                 fit$logscale$smp$loo())

loo::loo_compare(tight_fit_gb2$loo(), 
                 fit$gb2$smp$loo())

loo::loo_compare(tight_fit_pareto$loo(), 
                 fit$pareto$smp$loo())

