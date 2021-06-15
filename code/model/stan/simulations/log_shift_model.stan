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

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real<lower=0> sigma_e;
  real<lower=0> sigma_u;
  real<lower=0> nu;
  real<lower=-min(y)> lambda;
  
  real intercept;
  vector[K] beta;
  vector[D] u;
}
transformed parameters{
  vector[N] log_y = log(y + lambda);
  real s = skewness(log_y);
}

model {
  // define transformed outcome
  
  // Regression parameters
  intercept ~ student_t(3, 0, 10);
  beta ~ normal(0, 0.5);
  sigma_e ~ gamma(2, 10^-3);
  
  // Group effects
  sigma_u ~ gamma(2, 10^-3);
  u ~ normal(0, sigma_u); 
  
  // Raw parameters with a zero lower bound
  nu ~ gamma(2, 0.1);
  s ~ normal(0, 10^-3); 
  
  // Transformed Regression 
  for(n in 1:N){
  target += student_t_lpdf(log_y[n] |nu, intercept + X[n] * beta + u[domain[n]], sigma_e) 
                - log(y[n] + lambda);
  }
}

generated quantities{
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = student_t_lpdf(y[n] |nu, 
                        intercept + X[n] * beta + u[domain[n]], sigma_e) 
                        - log(y[n] + lambda);
  }
}

