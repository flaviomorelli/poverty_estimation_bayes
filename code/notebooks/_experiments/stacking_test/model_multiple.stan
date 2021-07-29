data {
#include data_abstract.stan
}

parameters {
#include param_abstract.stan
}

transformed parameters{
  vector[N] mu = alpha + X * beta;
}

model {
  alpha ~ normal(0, 10);
  beta ~ normal(0, 5);
  sigma ~ gamma(2, 0.2);
  y ~ normal(mu, sigma);
}

generated quantities {
#include gqs.stan
}
