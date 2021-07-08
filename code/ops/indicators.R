# FGT functions

poverty_line <- function(x) 0.6 * median(x)

hcr <- function(y, group, t) as.matrix(by(y, group, 
                                          function(x) mean(x <= t)))

pgap <- function(y, group, t) as.matrix(by(y, group, 
                                          function(x) mean(((t-x)/t) * (x <= t))))

fgt <- function(y_pred, group_id, n_groups, type = "hcr"){
  if(!(type %in% c("hcr", "pgap"))) 
    stop(stringr::str_c("`type` argument has to be `hcr` or `pgap`! Passed: ", type))
  
  result <- matrix(nrow = n_groups, 
                   ncol = ncol(y_pred))
  
  # List with indicator functions
  indicator <- list(hcr = hcr, 
                    pgap = pgap)
  for(i in 1:ncol(y_pred)){
    t <- poverty_line(y_pred[ , i])
    result[ , i] <- indicator[[type]](y_pred[ ,i], group_id, t)
  }
  return(result)
}

hb_indicator <- function(x) apply(x, MARGIN = 1, mean)

hb_list <- function(hcr_pred, scenarios){
  result <- list()
  
  for(scenario in scenarios){
    for(type in c("smp", "smp_miss")){
      result[[scenario]][[type]] <- hb_indicator(t(hcr_pred[[scenario]][[type]]))
    }
  }
  return(result)
}

indicator_list <- function(data, y_pred, scenarios, indicator){
  result <- list()
  
  for(scenario in scenarios){
    message(stringr::str_c("Calculating ", indicator, " for ", scenario, " scenario."))
    
    group <- data[[scenario]][["pop"]][["group_id"]]
    
    for(smp_type in c("smp", "smp_miss")){
      message(stringr::str_c("...calculating ", smp_type))
      result[[scenario]][[smp_type]] <- t(fgt(t(y_pred[[scenario]][[smp_type]]),
                                              group, 
                                              max(group), 
                                              type = indicator))
    }
  }
  
  return(result)
}

indicator_pop_list <- function(data, scenarios, type){
  indicator <- list(hcr = hcr, 
                    pgap = pgap)
  
  result <- list()
  
  for(scenario in scenarios){
    data_pop <- data[[scenario]][["pop"]]
    result[[scenario]][["pop"]] <- as.vector(indicator[[type]](
      data_pop$y, 
      group = factor(data_pop$group_id), 
      t = poverty_line(data_pop$y)
    ))
  }
  
  return(result)
}

# Diagnostics

indicator_bias <- function(ind_pred, ind_pop){
  col_bias <- apply(ind_pred, MARGIN = 2, FUN = function(x) (x - ind_pop)) 
  avg_bias <- (apply(col_bias, MARGIN = 1, FUN = mean))
  return(avg_bias)
}

indicator_rmse <- function(ind_pred, ind_pop){
  col_sq_error <- apply(ind_pred, MARGIN = 2, FUN = function(x) (x - ind_pop)^2) 
  mse <- apply(col_sq_error, MARGIN = 1, FUN = mean)
  return(sqrt(mse))
}

indicator_diagnostics <- function(ind_pred, ind_pop){
  list(
    bias = indicator_bias(ind_pred, ind_pop),
    rmse = indicator_rmse(ind_pred, ind_pop),
    hb_sd = apply(ind_pred, MARGIN = 1, FUN = sd)
    )
}

diagnostics_list <- function(ind_pred, ind_pop, scenarios){
  result <- list()
  for(scenario in scenarios){
    result[[scenario]][["smp"]] <- indicator_diagnostics(
                                              t(ind_pred[[scenario]][["smp"]]), 
                                              ind_pop[[scenario]][["pop"]]
                                              )
    
    result[[scenario]][["smp_miss"]] <- indicator_diagnostics(
                                              t(ind_pred[[scenario]][["smp_miss"]]), 
                                              ind_pop[[scenario]][["pop"]]
                                              )
  }
  result
}

# EBP

ebp_indicators <- function(data, scenarios){
  
  result <-  list()
  
  fixed_formula <- y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + (1|group_id)
  
  for(scenario in scenarios){
    message("Estimating EBP for ", scenario)
    for(type in c("smp", "smp_miss")){
      message("...estimating for ", type)
      result[[scenario]][[type]] <- emdi::ebp(
        fixed_formula, 
        pop_data = data[[scenario]][["pop"]], 
        pop_domains = "group_id", 
        smp_data = data[[scenario]][[type]], 
        smp_domains = "group_id",
        transformation = "log",
        MSE = TRUE,
        boot_type = "wild"
      )
    }
  } 
  return(result)
}

compare_diagnostics <- function(ebp_diagnostics, 
                                   bayes_diagnostics, 
                                   scenarios, 
                                   FGT = "HCR"){
  result <- list()
  for(scenario in scenarios){
    for(type in c("smp", "smp_miss")){
      bayes_rmse <- bayes_diagnostics[[scenario]][[type]][["rmse"]]
      
      ebp_rmse <- ebp_diagnostics[[scenario]][[type]][["MSE"]]
      if(FGT == "HCR"){
        ebp_rmse <- sqrt(ebp_rmse[["Head_Count"]])
      }
      else if(FGT == "PGAP"){
        ebp_rmse <- sqrt(ebp_rmse[["Head_Count"]])
      }
      else{
        stop("Argument `FGT` takes values 'HCR' or 'PGAP'")
      }
      result[[scenario]][[type]] <- median(ebp_rmse - bayes_rmse)
    }
      
  }
  return(result)
}
