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
  matrix[N, K] X;
  int domain[N];
}

transformed data{
  vector[N] y = X[ , 1];
}

parameters {
  real intercept;
  real<lower=2> nu;
  real lambda;
  
  real<lower=0> sigma;
  real<lower=0> sigma_u;
  vector[D] u_tilde;
  
  vector[K] beta;
}
transformed parameters{
  vector[N] log_y = log(y + lambda);
  real s = skewness(log_y);
  vector[D] u = u_tilde * sigma_u;
  real sigma_e = sigma * sqrt(nu - 2 / nu);
}


generated quantities {
  real y_pred[N];
  
  for(n in 1:N){
    y_pred[n] = exp(student_t_rng(nu, intercept + X[n] * beta + u[domain[n]], sigma_e)) - lambda;
  }
}