library(tidyverse)
library(cmdstanr)
library(posterior)

# Load data
source(file.path("dataloader", "data_cleaning.R"))

# Load numeric functions
source(file.path("ops", "indicators.R"))

# Set up map
mexico <- raster::shapefile(file.path("data", "mexico_shapefile", "Muni_2012gw.shp"))
mexico@data[["CVE_MUN"]] <- mexico@data[["CVE_MUN"]] %>% 
  as.character() %>% as.numeric() 

guerrero_obs <- mexico@data[["CVE_ENT"]] == "12"
guerrero_df <- fortify(mexico[guerrero_obs, ], region = "CVE_MUN")

sar_model <- cmdstan_model(file.path("model", "stan", 
                                     "final_sar.model.stan"))

combined_list <- c(mcs_one_hot_strat, census_one_hot_strat)

hb_fit <- sar_model$sample(combined_list, 
                 parallel_chains = 4, 
                 iter_warmup = 1200, 
                 iter_sampling = 300)

hb_fit$cmdstan_diagnose()


hb_output <- read_cmdstan_csv(hb_fit$output_files(),
                              variables = "y_pred")

hb_pred <- hb_output$post_warmup_draws %>% as_draws_matrix()
write.csv(hb_pred, file.path("data", "predictions", "hb_pred.csv"))

weighted_hcr(as.numeric(t(hb_pred)[, 1]) %>% unlist, 
             factor(census_one_hot_strat$mun), 
             1000, 
             census_one_hot_strat$factor)

hb_hcr <- fgt(t(hb_pred), 
    census_one_hot_strat$mun, 
    max(census_one_hot_strat$mun), 
    type = "weighted_hcr", 
    w = census_one_hot_strat$factor)

apply(hb_hcr, MARGIN = 1, sd)

write.csv(hb_hcr, file.path("data", "predictions", "hb_hcr.csv"))

hb_pgap <- fgt(t(hb_pred), 
    census_one_hot_strat$mun, 
    max(census_one_hot_strat$mun), 
    type = "weighted_pgap", 
    w = census_one_hot_strat$factor)

write.csv(hb_pgap, file.path("data", "predictions", "hb_pgap.csv"))

ebp_indicators <- emdi::ebp(
  ictpc ~ pcpering + actcom_pc + bienes_pc +  jexp + jedad + pcocup + 
    jsector_Primario + jsector_Secundario + jsector_Terciario + id_men_TRUE. +
    trabinusual_TRUE. +  ingresoext_TRUE. + pob_ind_TRUE., 
  pop_data = census_one_hot, 
  pop_domains = "mun", 
  smp_data = mcs_one_hot, 
  smp_domains = "mun",
  transformation = "box.cox",
  MSE = TRUE,
  boot_type = "wild"
)

write.csv(ebp_indicators$ind, file.path("data", "predictions", "ebo_ind.csv"))
write.csv(sqrt(ebp_indicators$MSE[, 2:11]), file.path("data", "predictions", "ebo_rmse.csv"))

ggplot(data = guerrero_df) +
  geom_polygon(aes(long, lat, group = group), color = "black") +
  theme_void() 

