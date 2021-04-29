data {
  int<lower=0> N;
  int<lower=0> K;
  matrix[N, K] X;
  vector[N] y;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real alpha;
  vector[K] beta;
  
  real<lower=0> sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  alpha ~ normal(0, 5);
  sigma ~ gamma(2, 0.2);
  beta ~ normal(0, 1);
  y ~ normal(alpha + X * beta, sigma);
}

generated quantities {
  real y_pred[N];
  real log_lik[N];
  
  for(n in 1:N){
    y_pred[n] = normal_rng(alpha + X[n, ] * beta, sigma);
    log_lik[n] = normal_lpdf(y[n]| alpha + X[n, ] * beta, sigma);
  }
}

