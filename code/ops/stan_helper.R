source(file.path("ops", "imputation.R"))

# Function to combine data when there are out-of-sample observations
pop_data_miss <- function(pop_data, smp_data){
  pop_data$D_tot <- pop_data$D
  pop_data$in_sample <- smp_data$in_sample
  return(pop_data)
}

stan_fit <- function(model, data, chains = 2){ 
  model$sample(
    data = data,
    chains = chains, 
    parallel_chains = chains
  )
}

scenario_fit <- function(data, model, scenario) lapply(
  list(
    smp = data[[scenario]][["smp_stan"]],
    smp_miss = data[[scenario]][["smp_miss_stan"]]),
  stan_fit, model = model
)

get_pred <- function(model, fit, pop, smp, type = "std", impute = TRUE){
  if(type == "std")
    y_pred <- get_pred_std(model, fit, pop, smp)
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

get_pred_std <- function(model, fit, pop, smp){
  posterior::as_draws_matrix(
    model$generate_quantities(
      fit, 
      data = pop, 
      seed = seed, 
      parallel_chains = 2
    )$draws() 
  )
}

get_pred_miss <- function(model, fit, pop, smp){
  posterior::as_draws_matrix(
    model$generate_quantities(
      fit, 
      data = pop_data_miss(pop, smp), 
      seed = seed, 
      parallel_chains = 2
    )$draws("y_pred") 
  )
}