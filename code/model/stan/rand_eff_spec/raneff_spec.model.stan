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

parameters {
  real<lower=0> sigma;
  real<lower=0> sigma_u;
  real<lower=2> nu;
  real<lower=-min(y)> lambda;
  
  real intercept;
  vector[K] beta;
  vector[D] u_tilde;
}
transformed parameters{
  vector[N] log_y = log(y + lambda);
  real s = skewness(log_y) * 1000;
  vector[D] u = u_tilde * sigma_u;
  real sigma_e = sigma * sqrt(nu - 2 / nu);
}

model {
  // define transformed outcome
  
  // Regression parameters
  intercept ~ normal(4, 10);
  beta ~ normal(0, 0.3);
  sigma ~ gamma(2, 1);
  
  // Group effects
  sigma_u ~ gamma(2, 1);
  u_tilde ~ std_normal(); 
  
  // Raw parameters with a zero lower bound
  nu ~ gamma(2, 0.1);
  s ~ normal(0, 1); 
  
  // Transformed Regression 
  real mu[N]; 
  for(n in 1:N)
    mu[n] = intercept + X[n] * beta + u[domain[n]];
  
  // Remember: - log_y is also the Jacobian correction
  target += student_t_lpdf(log(y)|nu, mu, sigma_e) - log(y);
}

generated quantities{
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = student_t_lpdf(log_y[n] |nu, 
                        intercept + X[n] * beta + u[domain[n]], 
                        sigma_e) 
                        - log(y[n] + lambda);
  }
}

