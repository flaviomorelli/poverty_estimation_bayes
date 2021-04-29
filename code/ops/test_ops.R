library(testthat)


# trafo
{
  
source("ops/trafo.R")
  

test_that(
  "Absolute skewness is approx. zero after optimizing",
  {
    N <- 10000
    gamma <- rgamma(N, 2, 0.1)
    lognormal <- rlnorm(N, 8, 3)
    pareto_mixture <- Pareto::rPareto(N, 1, 5) * 10 +
                rnorm(N, 5, 2)
    expect_equal(round(find_shift(gamma)$minimum), 0)
    expect_equal(round(find_shift(lognormal)$minimum), 0)
    expect_equal(round(find_shift(pareto_mixture)$minimum), 0)
  }
)

}
  

