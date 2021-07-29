data {
#include data_abstract.stan
}

parameters {
  real alpha;
  real sigma;
}

transformed parameters{
  vector[N] mu;
  for(n in 1:N)
    mu[n] = alpha;
}

model {
  alpha ~ normal(0, 10);
  y ~ normal(mu, sigma);
}

generated quantities {
#include gqs.stan
}

