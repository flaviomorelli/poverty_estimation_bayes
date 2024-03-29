# Load seed
source(file.path("config", "sim_config.R"))

# Load functions for imputation
source(file.path("ops", "imputation.R"))

iteration_loop <- function(FUN, 
                           scenarios = c("logscale", "gb2", "pareto"),
                           types = c("smp", "smp_miss"),
                           ...){
  
  result <- list()
  for(scenario in scenarios){
    message(str_c("Scenario: ", scenario))
    for(type in types){
      result[[scenario]][[type]] <- FUN(scenario, type, ...)
    }
  }
  return(result)
}

# Function to combine data when there are out-of-sample observations
pop_data_miss <- function(pop_data, smp_data){
  pop_data$D_tot <- pop_data$D
  pop_data$in_sample <- smp_data$in_sample
  return(pop_data)
}


stan_fit <- function(model, data, chains, 
                     iter_warmup, iter_sampling){ 
  model$sample(
    data = data,
    chains = chains, 
    parallel_chains = chains,  
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling
  )
}


scenario_fit <- function(data, model, scenario, chains = 2, 
                         iter_warmup = 1000, iter_sampling = 1000){
  lapply(
    list(
      smp = data[[scenario]][["smp_stan"]],
      smp_miss = data[[scenario]][["smp_miss_stan"]]),
    stan_fit, 
    model = model,
    chains = chains, 
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling
  )
}


scenario_pred <- function(data, gq_model, model_fit, scenario, regressors, type = "std"){
  pop <-  data[[scenario]][["pop_stan"]] 
  if(type == "std")
    smp <-  data[[scenario]][["smp_stan"]]
  else if(type == "missing")
    smp <-  data[[scenario]][["smp_miss_stan"]]
  else
    stop(stringr::str_c("Type ", type, " not recognized. Choose `std` or `missing`"))
  
  get_pred(gq_model, 
           model_fit, 
           pop = pop,
           smp = smp, 
           regressors = regressors, #define whether sample or pop regression
           type = type) 
}


scenario_pred_list <- function(data, gq_model, gq_miss_model, fit_list, scenarios, regressors = "smp"){
  result <- list()
  for(scenario in scenarios){
    result[[scenario]][["smp"]] <- scenario_pred(data, 
                                                 gq_model, 
                                                 fit_list[[scenario]][["smp"]], 
                                                 scenario, 
                                                 regressors = regressors, 
                                                 type = "std")
    
    result[[scenario]][["smp_miss"]] <- scenario_pred(data, 
                                                 gq_miss_model, 
                                                 fit_list[[scenario]][["smp_miss"]], 
                                                 scenario, 
                                                 regressors = regressors, 
                                                 type = "missing")
  }
  return(result)
}


get_pred <- function(model, fit, pop, smp, regressors, type = "std", impute = TRUE){
  if(type == "std")
    y_pred <- get_pred_std(model, fit, pop, smp, regressors)
  else if(type == "missing")
    y_pred <- get_pred_miss(model, fit, pop, smp)
  else
    stop("Type not supported")
  
  extremes_diagnostics(smp[["y"]], y_pred)
  
  if(impute){
    message("Imputing extremely high predictions. Pass `impute = FALSE` to avoid imputation.")
    y_pred <- impute_max(smp[["y"]], y_pred)
  }
  
  message("Imputation complete")
  
  return(y_pred)
}


get_pred_std <- function(model, fit, pop, smp, regressors){
  if(regressors == "smp") data = smp
  else data = pop
  return(posterior::as_draws_matrix(
    model$generate_quantities(
      fit, 
      data = pop, 
      seed = seed, 
      parallel_chains = 4
    )$draws() 
  ))
}


get_pred_miss <- function(model, fit, pop, smp){
  posterior::as_draws_matrix(
    model$generate_quantities(
      fit, 
      data = pop_data_miss(pop, smp), 
      seed = seed, 
      parallel_chains = 4
    )$draws("y_pred") 
  )
}