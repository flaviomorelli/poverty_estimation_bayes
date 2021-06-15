data {
  int<lower=0> N;
  int<lower=0> K;
  int<lower=0> D;
  matrix[N, K] X;
  int domain[N];
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real<lower=0> sigma_e;
  real<lower=0> sigma_u;
  real<lower=0> nu;
  real lambda;
  
  real intercept;
  vector[K] beta;
  vector[D] u;
}

generated quantities {
  real y_pred[N];
  
  for(n in 1:N){
    y_pred[n] = exp(student_t_rng(nu, intercept + X[n] * beta + u[domain[n]], sigma_e)) - lambda;
  }
}