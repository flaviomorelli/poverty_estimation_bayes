//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  int<lower=0> K;
  vector[N] x;
  vector[N] y;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real intercept;
  real beta;
  real<lower=0> omega;
  real alpha;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  intercept ~ normal(0, 10);
  beta ~ normal(10, 5);
  omega ~ gamma(2, 2);
  alpha ~ gamma(2, 0.01);
  y ~ skew_normal(intercept + beta * x, omega, alpha);
}

generated quantities {
  vector[N] log_lik;
  vector[N] y_pred;
  for (n in 1:N) {
    log_lik[n] = skew_normal_lpdf(y[n] | intercept + beta * x[n], omega, alpha);
  }
  
  for (n in 1:N) {
    y_pred[n] = skew_normal_rng(intercept + beta * x[n], omega, alpha);
  }
  
}
