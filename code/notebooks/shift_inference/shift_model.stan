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
  vector[N] x;
  vector<lower = 0>[N] y;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
  real<lower=0> nu_raw;
  real<lower=0> lambda_raw;
}

transformed parameters{
  real nu = 2 + nu_raw;
  real lambda = lambda_raw - min(y);
  vector[N] y_log = log(y + lambda);
  real s = skewness(y_log);
}

model {
  // define transformed outcome
  
  // Regression parameters
  alpha ~ student_t(3, 0, 100);
  beta ~ student_t(3, 0, 5);
  sigma ~ gamma(2, 10^-3);
  
  // Raw parameters with a zero lower bound
  lambda_raw ~ gamma(2, 10^-4);
  nu_raw ~ gamma(2, 0.1);
  s ~ normal(0, 10^-3); // This is the most important bit!
  
  // Transformed Regression 
  target += student_t_lpdf(y_log |nu_raw, alpha + beta * x, sigma) 
                - log(y + lambda);
}

generated quantities{
  vector[N] y_pred;
  
  for(n in 1:N)
    y_pred[n] = exp(student_t_rng(nu_raw, alpha + beta * x[n], sigma)) - lambda;
}

