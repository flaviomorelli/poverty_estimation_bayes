library(tidyverse)
library(cmdstanr)
library(posterior)


# Load data
source(file.path("dataloader", "data_cleaning.R"))

# Load numeric functions
source(file.path("ops", "indicators.R"))

# Load aggregated data
mun_data <- readstata13::read.dta13(file.path("data", "mexico", 
                                              "base_2donivel_Nacional_2010.dta")) %>%
  as_tibble() %>% 
  filter(nom_ent == "Guerrero") %>% 
  select(mun, pobtot) %>% 
  mutate(id = as.character(as.numeric(mun)), rel_pop = pobtot / sum(pobtot))


# Set up map
mexico <- raster::shapefile(file.path("data", "mexico_shapefile", "Muni_2012gw.shp"))
mexico@data[["CVE_MUN"]] <- mexico@data[["CVE_MUN"]] %>% 
  as.character() %>% as.numeric() 

guerrero_obs <- mexico@data[["CVE_ENT"]] == "12"
guerrero_df <- fortify(mexico[guerrero_obs, ], region = "CVE_MUN")

# Generating indicators
sar_model <- cmdstan_model(file.path("model", "stan", 
                                     "final_sar.model.stan"))

base_model <- cmdstan_model(file.path("model", "stan", 
                                      "final_base.model.stan"))

combined_list <- c(mcs_one_hot_strat, census_one_hot_strat)

hb_fit <- sar_model$sample(combined_list, 
                 parallel_chains = 4, 
                 iter_warmup = 1200, 
                 iter_sampling = 300)

base_fit <- base_model$sample(combined_list, 
                           parallel_chains = 4, 
                           iter_warmup = 1200, 
                           iter_sampling = 300)


hb_output <- read_cmdstan_csv(hb_fit$output_files(),
                              variables = "y_pred")

base_output <- read_cmdstan_csv(base_fit$output_files(),
                              variables = "y_pred")


write.csv(hb_output$post_warmup_draws %>% as_draws_matrix(), 
          file.path("data", "predictions", "hb_pred.csv"))

write.csv(base_output$post_warmup_draws %>% as_draws_matrix(), 
          file.path("data", "predictions", "base_pred.csv"))
# Load stored predictions
hb_pred <- data.table::fread(file = file.path("data", "predictions", "hb_pred.csv")) %>% 
  as.matrix %>% .[ , -1]

base_pred <- data.table::fread(file = file.path("data", "predictions", "base_pred.csv")) %>% 
  as.matrix %>% .[ , -1]


hb_hcr <- fgt(t(hb_pred), 
    census_one_hot_strat$mun, 
    max(census_one_hot_strat$mun), 
    type = "hcr", 
    w = census_one_hot_strat$factor)

base_hcr <- fgt(t(base_pred), 
              census_one_hot_strat$mun, 
              max(census_one_hot_strat$mun), 
              type = "hcr", 
              w = census_one_hot_strat$factor)

write.csv(hb_hcr, file.path("data", "predictions", "hb_hcr.csv"))
write.csv(base_hcr, file.path("data", "predictions", "base_hcr.csv"))

hb_pgap <- fgt(t(hb_pred), 
    census_one_hot_strat$mun, 
    max(census_one_hot_strat$mun), 
    type = "pgap", 
    w = census_one_hot_strat$factor)

base_pgap <- fgt(t(base_pred), 
               census_one_hot_strat$mun, 
               max(census_one_hot_strat$mun), 
               type = "pgap", 
               w = census_one_hot_strat$factor)

write.csv(hb_pgap, file.path("data", "predictions", "hb_pgap.csv"))
write.csv(base_pgap, file.path("data", "predictions", "base_pgap.csv"))

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

write.csv(ebp_indicators$ind, file.path("data", "predictions", "ebp_ind.csv"))
write.csv(sqrt(ebp_indicators$MSE[, 2:11]), file.path("data", "predictions", "ebp_rmse.csv"))

# Loading stored indicators
hb_hcr <- read_csv(file.path("data", "predictions", "hb_hcr.csv") ) %>% select(-X1)
hb_pgap <- read_csv(file.path("data", "predictions", "hb_pgap.csv"))%>% select(-X1)
base_hcr <- read_csv(file.path("data", "predictions", "base_hcr.csv") ) %>% select(-X1)
base_pgap <- read_csv(file.path("data", "predictions", "base_pgap.csv"))%>% select(-X1)
ebp_ind <- read_csv(file.path("data", "predictions", "ebp_ind.csv")) %>% 
  mutate(id = as.character(Domain)) %>% 
  select(id, Head_Count, Poverty_Gap) %>% 
  rename(ebp_hcr_mean = Head_Count, ebp_pgap_mean = Poverty_Gap)
ebp_rmse <- read_csv(file.path("data", "predictions", "ebp_rmse.csv")) %>% 
  mutate(id = as.character(X1)) %>% 
  select(id, Head_Count, Poverty_Gap) %>% 
  rename(ebp_hcr_rmse = Head_Count, ebp_pgap_rmse = Poverty_Gap)

hcr <- data.frame(hb_hcr_mean = apply(hb_hcr, MARGIN = 1, mean),
                  hb_hcr_sd = apply(hb_hcr, MARGIN = 1, sd),
                  base_hcr_mean = apply(base_hcr, MARGIN = 1, mean),
                  base_hcr_sd = apply(base_hcr, MARGIN = 1, sd)) %>% 
  rowid_to_column(var = "id") %>% 
  mutate(id = as.character(id))

pgap <- data.frame(hb_pgap_mean = apply(hb_pgap, MARGIN = 1, mean),
                   hb_pgap_sd = apply(hb_pgap, MARGIN = 1, sd),
                   base_pgap_mean = apply(base_pgap, MARGIN = 1, mean),
                   base_pgap_sd = apply(base_pgap, MARGIN = 1, sd))%>% 
  rowid_to_column(var = "id") %>% 
  mutate(id = as.character(id))

ind_diff <- data.frame(id = hcr$id, 
                       HCR = (ebp_ind$ebp_hcr_mean - hcr$hb_hcr_mean),
                       PGAP = (ebp_ind$ebp_pgap_mean - pgap$hb_pgap_mean))

diff_plot <- ggplot(data = ind_diff %>% 
         gather("Indicator", "val", -id), 
       aes(x = as.numeric(id), y = val, color = Indicator)) + 
  geom_line() + 
  labs(x = "Municipality", y = "", color="") +
  theme_minimal() + 
  theme(text = element_text(size = 14))

ggsave(file.path("notebooks", "final_model_indicators", "diff.png"), 
       plot = diff_plot,
       width = 16, 
       height = 9, 
       unit = "cm")

guerrero_ind_df <- guerrero_df %>% 
  inner_join(hcr, by = "id") %>% 
  inner_join(pgap, by = "id") %>% 
  inner_join(ebp_ind, by = "id") %>% 
  inner_join(ebp_rmse, by = "id") %>% 
  inner_join(ind_diff, by = "id")

# Generate plots

guerrero_map <- function(data, variable, legend){
  ggplot(data = data) +
    geom_polygon(aes_string("long", "lat", group = "group", fill = variable)) +
    theme_void() +
    labs(fill = legend) +
    scale_fill_gradient(low = "yellow", high = "red") +
    theme(legend.title = element_text(size = 14),
          legend.text = element_text(size = 14))
}

hb_hcr_mean_plot <- guerrero_map(guerrero_ind_df, "hb_hcr_mean", "HCR") + 
  scale_fill_gradient(low = "yellow", high = "red", limits = c(0, 0.78))
hb_hcr_sd_plot <- guerrero_map(guerrero_ind_df, "hb_hcr_sd", "S.D. HCR")
  
hb_pgap_mean_plot <- guerrero_map(guerrero_ind_df, "hb_pgap_mean", "PGAP")+ 
  scale_fill_gradient(low = "yellow", high = "red", limits = c(0, 0.5))
hb_pgap_sd_plot <- guerrero_map(guerrero_ind_df, "hb_pgap_sd", "S.D. PGAP")
hb_hcr_cv_plot <- guerrero_map(guerrero_ind_df, "hb_hcr_sd/hb_hcr_mean", "CV HCR")
hb_pgap_cv_plot <- guerrero_map(guerrero_ind_df, "hb_pgap_sd/hb_pgap_mean", "CV PGAP")

ebp_hcr_mean_plot <- guerrero_map(guerrero_ind_df, "ebp_hcr_mean", "HCR")+ 
  scale_fill_gradient(low = "yellow", high = "red", limits = c(0, 0.78))
ebp_hcr_rmse_plot <- guerrero_map(guerrero_ind_df, "ebp_hcr_rmse", "RMSE HCR")
ebp_pgap_mean_plot <- guerrero_map(guerrero_ind_df, "ebp_pgap_mean", "PGAP")+ 
  scale_fill_gradient(low = "yellow", high = "red", limits = c(0, 0.5))
ebp_pgap_rmse_plot <- guerrero_map(guerrero_ind_df, "ebp_pgap_rmse", "RMSE PGAP")
ebp_hcr_cv_plot <- guerrero_map(guerrero_ind_df, "ebp_hcr_rmse/ebp_hcr_mean", "CV HCR")
ebp_pgap_cv_plot <- guerrero_map(guerrero_ind_df, "ebp_pgap_rmse/ebp_pgap_mean", "CV PGAP")

hcr_diff_plot <- guerrero_map(guerrero_ind_df, "hcr_diff", "HCR DIFF")
pgap_diff_plot <- guerrero_map(guerrero_ind_df, "pgap_diff", "PGAP DIFF")

save_plot <- purrr::partial(ggsave, width = 16, height = 9, unit = "cm")

graph_path <- file.path("notebooks", "final_model_indicators")

save_plot(filename = str_c(graph_path, "/hb_hcr_mean.png"), hb_hcr_mean_plot)
save_plot(filename = str_c(graph_path, "/hb_hcr_sd.png"), hb_hcr_sd_plot)
save_plot(filename = str_c(graph_path, "/hb_pgap_mean.png"), hb_pgap_mean_plot)
save_plot(filename = str_c(graph_path, "/hb_pgap_sd.png"), hb_pgap_sd_plot)
save_plot(filename = str_c(graph_path, "/hb_hcr_cv.png"), hb_hcr_cv_plot)
save_plot(filename = str_c(graph_path, "/hb_pgap_cv.png"), hb_pgap_cv_plot)

save_plot(filename = str_c(graph_path, "/ebp_hcr_mean.png"), ebp_hcr_mean_plot)
save_plot(filename = str_c(graph_path, "/ebp_hcr_rmse.png"), ebp_hcr_rmse_plot)
save_plot(filename = str_c(graph_path, "/ebp_pgap_mean.png"), ebp_pgap_mean_plot)
save_plot(filename = str_c(graph_path, "/ebp_pgap_rmse.png"), ebp_pgap_rmse_plot)
save_plot(filename = str_c(graph_path, "/ebp_hcr_cv.png"), ebp_hcr_cv_plot)
save_plot(filename = str_c(graph_path, "/ebp_pgap_cv.png"), ebp_pgap_cv_plot)

save_plot(filename = str_c(graph_path, "/hcr_diff.png"), hcr_diff_plot)
save_plot(filename = str_c(graph_path, "/pgap_diff.png"), pgap_diff_plot)

ggplot() +
  geom_line(data = hcr, mapping = aes(x = as.numeric(id), y = hb_hcr_mean)) +
  geom_line(data = ebp_ind, mapping = aes(x = as.numeric(id), y =ebp_hcr_mean), color = "blue")

# Benchmarking
emdi_direct <- emdi::direct("ictpc",
             as.data.frame(mcs),
             smp_domains = "mun",
             )$ind %>% 
  rename(id = Domain) 

benchmarking_df <- emdi_direct %>% 
  inner_join(hcr, by = "id") %>% 
  inner_join(pgap, by = "id") %>% 
  inner_join(ebp_ind, by = "id") %>% 
  inner_join(mun_data, by = "id") %>% 
  mutate(rel_pop_2 = pobtot / sum(pobtot))


weighted.mean(benchmarking_df$Head_Count, w = benchmarking_df$rel_pop_2)
weighted.mean(benchmarking_df$hb_hcr_mean, w = benchmarking_df$rel_pop_2)
weighted.mean(benchmarking_df$ebp_hcr_mean, w = benchmarking_df$rel_pop_2)

weighted.mean(benchmarking_df$Poverty_Gap, w = benchmarking_df$rel_pop_2)
weighted.mean(benchmarking_df$hb_pgap_mean, w = benchmarking_df$rel_pop_2)
weighted.mean(benchmarking_df$ebp_pgap_mean, w = benchmarking_df$rel_pop_2)

