data {
#include data_abstract.stan
}

parameters {
#include param_abstract.stan
  vector[n_group] u;
}

transformed parameters{
  vector[N] mu;
  for(n in 1:N)
    mu[n] = alpha + X[n, ] * beta + u[group[n]];
}

model {
  alpha ~ normal(0, 10);
  beta ~ normal(0, 5);
  u ~ normal(0, 5);
  sigma ~ gamma(2, 0.2);
  for(n in 1:N){
    y[n] ~ normal(mu[n], sigma);
  }
}

generated quantities {
#include gqs.stan
}
