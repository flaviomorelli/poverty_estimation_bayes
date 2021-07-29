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
  real<lower = 0> nu;
  real<lower = 0> sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  intercept ~ normal(0, 5);
  beta ~ normal(1, 2);
  sigma ~ gamma(2, 1);
  nu ~ gamma(2, 0.2);
  y ~ student_t(nu + 2, intercept + beta * x,sigma);
}

generated quantities {
  vector[N] log_lik;
  vector[N] y_pred;
  for (n in 1:N) {
    log_lik[n] = student_t_lpdf(y[n] | nu + 2, intercept + beta * x[n], sigma);
  }
  for (n in 1:N) {
    y_pred[n] = student_t_rng(nu + 2, intercept + beta * x[n], sigma);
  }
}
