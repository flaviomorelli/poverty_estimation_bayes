data {
  int<lower=0> N;
  int<lower=0> K;
  matrix[N, K] X;
  vector[N] y;
  int<lower=1> D; // number of grouping levels
  int group[N];
}

transformed data{
  vector[N] Z = rep_vector(1, N);
}

parameters {
  real intercept;
  vector[D] std_reff;
  real<lower=0> sigma_reff;
  vector[K] beta;
  real<lower=0> sigma;
}

transformed parameters{
  vector[D] reff = std_reff * sigma_reff;
}

model {
  vector[N] mu = intercept + rep_vector(0.0, N);
  for(n in 1:N)
    mu[n] += intercept + reff[group[n]] * Z[n] + X[n, ] * beta;
  intercept ~ student_t(3, 0, 2.5);
  std_reff ~ student_t(3, 0, 2.5);
  beta ~ normal(0, 2);
  sigma ~ gamma(2, 0.1);
  sigma_reff ~ gamma(2, 0.1);   
  y ~ normal(mu, sigma);
}

