functions{
  real skewness(vector y){
    int N = num_elements(y);
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
  vector<lower=0>[N] y;
  matrix[N, K] X;
  int domain[N];
}

parameters {
  real intercept;
  real<lower=2> nu;
  real<lower=-min(y) + 0.1> lambda;
  
  real<lower=0> sigma;
  real<lower=0> sigma_u;
  vector[D] u_tilde;
  cholesky_factor_corr[D] L_Omega;
  
  vector[K] beta;
}
transformed parameters{
  vector[N] log_y = log(y + lambda);
  real s = skewness(log_y) * 1000;
  real sigma_e = sigma * sqrt(nu - 2 / nu);
  matrix[D, D] L_Sigma = diag_pre_multiply(rep_vector(sigma_u, D), L_Omega);
  vector[D] u = L_Sigma * u_tilde;
}

model {
  intercept ~ normal(4, 3);
  beta ~ normal(0, 0.2);
  sigma ~ gamma(2, 10);
  
  sigma_u ~ gamma(2, 10);
  u_tilde ~ std_normal();
  L_Omega ~ lkj_corr_cholesky(7);
  
  // Shape parameters
  nu ~ gamma(2, 0.1);
  s ~ normal(0, 1); 
  
  // Likelihood
  vector[N] mu;
  for(n in 1:N)
    mu[n] = intercept + X[n] * beta + u[domain[n]];

  log_y ~ student_t(nu, mu, sigma_e);
  target += - log_y; // Jacobian adjustment
}

generated quantities{
  vector[N] log_lik;
  matrix[D, D] Sigma = multiply_lower_tri_self_transpose(L_Sigma);
  matrix[D, D] Omega = multiply_lower_tri_self_transpose(L_Omega);
  for (n in 1:N) {
    log_lik[n] = student_t_lpdf(log_y[n] |3,
                        intercept + X[n] * beta + u[domain[n]],
                        sigma_e)
                        - log_y[n];
  }
  
}

