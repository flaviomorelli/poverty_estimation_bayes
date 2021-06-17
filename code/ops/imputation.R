count_extremes <- function(y, y_rep, mode = "max"){
  cmp <- list("min" = function(x, y) x < y,
              "max" = function(x, y) x > y)
  func <- list("min" = min, 
               "max" = max)
  sum(cmp[[mode]](y_rep, func[[mode]](y))) / length(y_rep)
}

extremes_diagnostics <- function(y, y_rep){
  diagnostic <- list(max = count_extremes(y, y_rep, "max"),
       min = count_extremes(y, y_rep, "min"))
  
  message(str_c(round(diagnostic[["max"]] * 100, 3), 
                "% of predictions above data maximum."))
  message(str_c(round(diagnostic[["min"]] * 100, 3), 
                "% of predictions below data minimum."))
  return(diagnostic)
}

impute_max <- function(y, y_rep, quant_prob = 0.99){
  max_y <-  max(y)
  q_y <- quantile(y, quant_prob)
  apply(y_rep, 
        MARGIN = 2, 
        FUN = function(x) ifelse(x > max_y, 
                                 runif(1, q_y, max_y),
                                 x))
}
