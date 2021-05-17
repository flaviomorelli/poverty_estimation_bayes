data {
  int<lower=0> N;
  int<lower=0> K;
  vector[N] y;
  matrix[N, K] X;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  
  real alpha;
  vector[K] beta;
  real<lower=0> sigma;
  
  // Regularized horseshow parameters
  real<lower=0> c;
  // real<lower=0> tau;
  real<lower=0> lambda_0 [K];

}

transformed parameters{
  vector<lower=0>[K] lambda;
  real tau = 20.0 / N;
  for(k in 1:K)
    lambda[k] = c * lambda_0[k] / (c + tau^2 * lambda_0[k]);
}


model {
  alpha ~ normal(0, 10);
  lambda_0 ~ cauchy(0, 1);
  for(k in 1:K)
    beta[K] ~ normal(0, tau^2 * lambda[k]^2);
  c ~ inv_gamma(3.0/2.0, 3.0/2.0);
  y ~ normal(alpha + X * beta, sigma);
}



