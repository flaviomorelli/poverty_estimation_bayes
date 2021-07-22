functions{
  real skewness(vector y){
    int N = size(y);
    vector[N] yc = y - mean(y);
    real s_3 = sd(y)^3;
    real m_3 = mean(yc.^3);
    return m_3 / s_3;
  }
}

data {
  int<lower=0> N;
  int<lower=0> K;
  int<lower=0> D;
  vector[N] y;
  matrix[N, K] X;
  int domain[N];
}

// parameters {
//   real<lower=-min(y)> lambda;
// }
// 
// model {
//   vector[N] log_y = log(y + lambda);
//   real s = skewness(log_y);
//   s ~ normal(0, 10^-3);
// }

generated quantities{
  real intercept = student_t_rng(3, 0, 10);
  real sigma = gamma_rng(2, 0.75);
  real sigma_u = gamma_rng(2, 0.75);
  real nu_raw = gamma_rng(2, 1);
  real nu = nu_raw + 2.01;
  real sigma_e = sigma * sqrt(nu - 2 / nu);
  vector[K] beta;
  real u[D];
  real y_pred[N];
  for(k in 1:K)
    beta[k] = normal_rng(0, 0.5);
  for(d in 1:D)
    u[d] = normal_rng(0, sigma_u);
  for(n in 1:N)
    y_pred[n] = exp(student_t_rng(nu, intercept + X[n] * beta + u[domain[n]], 
        sigma_e)); 
}

