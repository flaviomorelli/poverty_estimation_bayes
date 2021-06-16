poverty_line <- function(x) 0.6 * median(x)

hcr <- function(y, group, t) as.matrix(by(y, group, 
                                          function(x) mean(x < t)))

pgap <- function(y, group, t) as.matrix(by(y, group, 
                                          function(x) mean((t-y)/t * (x < t))))

fgt <- function(y_pred, group_id, n_groups, type = "hcr"){
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
    rmse = indicator_rmse(ind_pred, ind_pop)
    )
}
