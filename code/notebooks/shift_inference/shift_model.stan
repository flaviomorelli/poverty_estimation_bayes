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
  real lambda;
  //real<lower=0> nu_raw;
  //real<lower=0> lambda_raw;
}

transformed parameters{
  //real nu = 2 + nu_raw;
  //real lambda = lambda_raw - min(y);
}


model {
  // define transformed outcome
  vector[N] y_log = log(y + lambda);
  
  // Regression parameters
  alpha ~ student_t(3, 0, 80);
  beta ~ student_t(3, 0, 10);
  sigma ~ gamma(2, 0.5);
  
  // Raw parameters with a zero lower bound
  lambda ~ normal(0, min(y)/2);
 // nu_raw ~ gamma(2, 0.2);
  
  // Trnasformed Regression 
  target += student_t_lpdf(y_log |3, alpha + beta * x, sigma) 
                - log(y - lambda);
}

