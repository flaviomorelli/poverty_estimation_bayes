read_sim <- function(path, sample = FALSE){
  data <- readr::read_csv(path) 
  if(sample)
    data <- dplyr::filter(data, sample == TRUE)
  
  dplyr::select(data, -c(ε, u, row_id, sample))
}

make_stan_data <- function(data){ 
  list(N = nrow(data), 
     K = 10, 
     D = max(data$group_id),
     y = data$y, 
     X = select(data, starts_with("x")), 
     domain = data$group_id) 
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

logscale_smp <- read_sim(logscale_path, sample = TRUE)
logscale_smp_stan <- make_stan_data(logscale_smp)

gb2_smp <- read_sim(gb2_path, sample = TRUE)
gb2_smp_stan <- make_stan_data(gb2_smp)

pareto_smp <- read_sim(pareto_path, sample = TRUE)
pareto_smp_stan <- make_stan_data(pareto_smp)

message("Simulations loaded!")

rm(base_path,
   logscale_path,
   gb2_path, 
   pareto_path,
   read_sim,
   make_stan_data)
