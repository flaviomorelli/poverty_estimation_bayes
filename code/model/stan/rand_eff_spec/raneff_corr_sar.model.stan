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
  matrix[D, D] W_tilde;
  int domain[N];
}

parameters {
  real intercept;
  real<lower=2> nu;
  real<lower=-min(y) + 0.1> lambda;
  
  real<lower=0> sigma;
  real<lower=0> sigma_u;
  vector[D] u_tilde;
  real<lower=-1, upper=1> rho;
  
  vector[K] beta;
}
transformed parameters{
  vector[N] log_y = log(y + lambda);
  real s = skewness(log_y);
  real sigma_e = sigma * sqrt(nu - 2 / nu);
  matrix[D, D] I = diag_matrix(rep_vector(1, D));
  matrix[D, D] Omega = crossprod(I - rho * W_tilde);
  vector[D] u = sigma_u * u_tilde;
}

model {
  intercept ~ normal(0, 5);
  beta ~ normal(0, 0.2);
  sigma ~ gamma(2, 7);
  
  sigma_u ~ gamma(2, 7);
  u_tilde ~ multi_normal_prec(rep_vector(0, D), Omega);
  rho ~ normal(0, 0.4);
  
  // Shape parameters
  nu ~ gamma(2, 0.1);
  s ~ normal(0, 0.01); 
  
  // Likelihood
  vector[N] mu;
  for(n in 1:N)
    mu[n] = intercept + X[n] * beta + u[domain[n]];

  log_y ~ student_t(nu, mu, sigma_e);
  target += - log_y; // Jacobian adjustment
}

generated quantities{
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = student_t_lpdf(log_y[n] |nu,
                        intercept + X[n] * beta + u[domain[n]],
                        sigma_e)
                        - log_y[n];
  }
  
}