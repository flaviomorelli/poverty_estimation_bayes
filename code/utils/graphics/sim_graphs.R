library(bayesplot)

# Load paths
source(file.path("config", "sim_config.R"))
source(file.path("ops", "stan_helper.R"))

# Global theme setup
bayesplot_theme_set(theme_minimal(base_size = 14) + theme(legend.position = "none"))

save_graph <- purrr::partial(ggplot2::ggsave, 
                             device = "png", 
                             width = 25, 
                             height = 7, 
                             units = "cm")

graph_list <- function(y, 
                       y_pred, 
                       n_pred = 100, 
                       alpha = 0.3){
  result <- list()

  result$dens <- ppc_dens_overlay(y, 
                                  y_pred[1:n_pred, ], size = 0.2) + xlim(c(0, 0.6 * max(y)))
  
  result$median_2d <- ppc_stat_2d(y, 
                                  y_pred, 
                                  stat = c("median", "IQR"), 
                                  alpha = alpha)
  
  result$mean_2d <- ppc_stat_2d(y, 
              y_pred, 
              stat = c("mean", "sd"), alpha = alpha)
  
  return(result)
} 



create_graphs <- function(data, y_pred, scenarios, graph_path = graph_path, name = "", from_brms = FALSE){
  for(scenario in scenarios){
    message(stringr::str_c("Creating graphs for ", scenario, " scenario."))
    if(from_brms){
      message("You are using data from brms")
      y_pred_smp <- brms::posterior_predict(y_pred[[scenario]][["smp"]])
      #y_pred_smp_miss <- t(brms::posterior_predict(y_pred[[scenario]][["smp_miss"]]))
    } 
    else{
      y_pred_smp <- y_pred[[scenario]][["smp"]]
      #y_pred_smp_miss <- y_pred[[scenario]][["smp_miss"]]
    }
    
    smp_list <- graph_list(data[[scenario]][["smp"]][["y"]], 
                           y_pred_smp)
    # smp_miss_list <- graph_list(data[[scenario]][["smp_miss"]][["y"]], 
    #                             y_pred_smp_miss)
    
    
    smp_plot <- bayesplot_grid(plots = smp_list, 
                               grid_args = list(ncol = length(smp_list)))
    # smp_miss_plot <- bayesplot_grid(plots = smp_miss_list, 
    #                                 grid_args = list(ncol = length(smp_list)))
    
    path <- file.path(graph_path, scenario)
    message("Saving graphs.")
    save_graph(filename = file.path(path, stringr::str_c(scenario, "_smp_", name, ".png")), 
               plot = smp_plot)
    # save_graph(filename = file.path(path, stringr::str_c(scenario, "_smp_miss_", name, ".png")), 
    #            plot = smp_miss_plot)
  }
}

skewness_graphs <- function(fit, scenarios){
  for(scenario in scenarios){
    message(stringr::str_c("Creating graphs for ", scenario, " scenario."))
    
    smp_plot <- mcmc_dens_chains(fit[[scenario]][["smp"]]$draws("s"))
    smp_miss_plot <- mcmc_dens_chains(fit[[scenario]][["smp_miss"]]$draws("s"))
    
    path <- file.path(graph_path, scenario)
    
    message("Saving graphs.")
    save_graph(filename = file.path(path, stringr::str_c(scenario, "_skew.png")), 
               plot = bayesplot_grid(smp_plot, 
                                     smp_miss_plot, grid_args = list(ncol = 2)))
  }
}

indicator_kde_graph <- function(pop, ebp, hb){
  ggplot() +
    geom_density(data = data.frame(y = pop), 
                 mapping = aes(y), 
                 color = "black", 
                 size = 1) +
    geom_density(data = data.frame(y = hb), 
                 mapping = aes(y), 
                 color = "blue", 
                 size = 1) + 
    geom_density(data = data.frame(y = ebp), 
                 mapping = aes(y), 
                 color = "#888888", 
                 linetype = "dashed", 
                 size = 1) + 
    theme_minimal()
}

# diagnostic_graphs <- 

  
  