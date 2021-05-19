
data {
  int<lower=0> N;
  vector[N] x;
  real y[N];
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real alpha;
  real beta;
  real lambda;
  real<lower=0> sigma;
}

// transformed parameters{
//   real y_log[N];
//   for(n in 1:N)
//     y_log[n] = log(y[n] + lambda);
// }


model {
  alpha ~ student_t(3, 0, 2.5);
  beta ~ student_t(3, 0, 2.5);
  sigma ~ gamma(2, 0.1);
  lambda ~ normal(0, 0.05);
  y ~ student_t(3, alpha + beta * x,sigma);
  // for(n in 1:N)
  //   target += log(fabs(y[n] + lambda));
}

