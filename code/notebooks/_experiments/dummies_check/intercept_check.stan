data {
  int<lower=0> N;
  int<lower=0> K;
  int<lower=1> D;
  matrix[N, K] X;
  vector[N] y;
  int group[N];
}

parameters {
  real alpha;
  vector[D] raneff;
  vector[K] beta;
  real<lower=0> sigma;
}

model {
  alpha ~ normal(0, 5);
  raneff ~ normal(0, 2);
  beta ~ normal(0, 2);
  sigma ~ gamma(2, 0.1);
  for(n in 1:N)
    y[n] ~ normal(alpha + raneff[group[n]] + X[n, ] * beta, sigma);
}

