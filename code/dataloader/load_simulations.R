read_sim <- function(path, type = "pop"){
  data <- readr::read_csv(path) 
  if(type == "sample")
    data <- dplyr::filter(data, sample == TRUE)
  else if(type == "sample_missing")
    data <- dplyr::filter(data, sample_miss == TRUE)
  
  dplyr::select(data, -c(ε, u, row_id, sample, sample_miss))
}

make_stan_data <- function(data, type = "normal", K = 10, n_domains = 50){ 
  stan_data <- list(N = nrow(data), 
     K = K, 
     D = length(unique(data$group_id)),
     y = data$y, 
     X = dplyr::select(data, starts_with("x")), 
     domain = data$group_id
     )
  
  if(type == "missing"){
    lookup <-  list()
    in_sample <- unique(data$group_id)
    
    for(i in seq_along(in_sample)){
      lookup[[as.character(in_sample[i])]] <- i
    }
    
    stan_data$in_sample <- as.numeric(1:n_domains %in% in_sample)
    stan_data$domain <-  sapply(data$group_id, 
                                function(x) lookup[[as.character(x)]])
    stan_data$original_id <- data$group_id
  }  

  return(stan_data)
}

base_path <- file.path("data", "simulations")
logscale_path <- file.path(base_path, "logscale.csv")
gb2_path <- file.path(base_path, "gb2.csv")
pareto_path <- file.path(base_path, "pareto.csv")

message("Loading simulations...")

# Population data sets

logscale_pop <- read_sim(logscale_path)
logscale_pop_stan <- make_stan_data(logscale_pop)

gb2_pop <- read_sim(gb2_path)
gb2_pop_stan <- make_stan_data(gb2_pop)

pareto_pop <- read_sim(pareto_path)
pareto_pop_stan <- make_stan_data(pareto_pop)

# Sample data sets

logscale_smp <- read_sim(logscale_path, type = "sample")
logscale_smp_stan <- make_stan_data(logscale_smp)

gb2_smp <- read_sim(gb2_path, type = "sample")
gb2_smp_stan <- make_stan_data(gb2_smp)

pareto_smp <- read_sim(pareto_path, type = "sample")
pareto_smp_stan <- make_stan_data(pareto_smp)

# Missing sample data sets
logscale_smp_miss <- read_sim(logscale_path, type = "sample_missing")
logscale_smp_miss_stan <- make_stan_data(logscale_smp_miss, 
                                         type = "missing")


gb2_smp_miss <- read_sim(gb2_path, type = "sample_missing")
gb2_smp_miss_stan <- make_stan_data(gb2_smp_miss, 
                                         type = "missing")

pareto_smp_miss <- read_sim(pareto_path, type = "sample_missing")
pareto_smp_miss_stan <- make_stan_data(pareto_smp_miss, 
                                         type = "missing")

# Data lists
sim_data <- list(
  logscale = list(
    pop = logscale_pop,
    pop_stan = logscale_pop_stan,
    smp = logscale_smp,
    smp_stan = logscale_smp_stan,
    smp_miss = logscale_smp_miss,
    smp_miss_stan = logscale_smp_miss_stan
  ),
  gb2 = list(
    pop = gb2_pop,
    pop_stan = gb2_pop_stan,
    smp = gb2_smp,
    smp_stan = gb2_smp_stan,
    smp_miss = gb2_smp_miss,
    smp_miss_stan = gb2_smp_miss_stan
  ),
  pareto = list(
    pop = pareto_pop,
    pop_stan = pareto_pop_stan,
    smp = pareto_smp,
    smp_stan = pareto_smp_stan,
    smp_miss = pareto_smp_miss,
    smp_miss_stan = pareto_smp_miss_stan
  )
)

message("Simulations loaded!")

rm(base_path,
   logscale_path,
   gb2_path, 
   pareto_path,
   read_sim,
   make_stan_data)

rm(logscale_pop,
   logscale_pop_stan,
   logscale_smp,
   logscale_smp_stan,
   logscale_smp_miss,
   logscale_smp_miss_stan,
   gb2_pop,
   gb2_pop_stan,
   gb2_smp,
   gb2_smp_stan,
   gb2_smp_miss,
   gb2_smp_miss_stan,
   pareto_pop,
   pareto_pop_stan,
   pareto_smp,
   pareto_smp_stan,
   pareto_smp_miss,
   pareto_smp_miss_stan
)
