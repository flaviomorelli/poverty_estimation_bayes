
#' Find shift term for log-shift transform 
#' by minimizing the absolute value of the skewness
#' @param data A vector with data to be transformed
#' @return A list with \code{minimizer}, the desired shift term, and the
#'    \code{minimum} of the absolute skewness of the log-shifted data 
find_shift <- function(data){
  if(any(data < 0))
    stop("The function only takes non-negative data")
  f <- function(s) abs(moments::skewness(log(data + s)))
  epsilon <- 0.001
  lower <- -min(data) + epsilon
  upper <- max(data)
  result <- optimize(f, c(lower, upper))
  return(list(minimizer = result$minimum,
              minimum = result$objective))
}



