data {
  int<lower=0> N;
  vector[N] y;
  vector[N] x;
  real temperature;
}

parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}

transformed parameters{
  vector[N] mu;
  mu = alpha + beta * x;
}

model {
  target += 1/temperature * normal_lpdf(y | mu, sigma);
  target += 1/temperature * normal_lpdf(alpha | 0, 10);
  target += 1/temperature * student_t_lpdf(beta |3, 0, 2);
  target += 1/temperature * gamma_lpdf(sigma | 2, 0.5);
}

generated quantities{
  real y_pred[N];
  for(n in 1:N)
    y_pred[n] = normal_rng(mu[n], sigma);
}
