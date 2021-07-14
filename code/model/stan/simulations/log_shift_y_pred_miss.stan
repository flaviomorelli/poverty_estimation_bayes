data {
  int<lower=0> N;
  int<lower=0> K;
  int<lower=0> D_tot;
  matrix[N, K] X;
  int<lower=0, upper=D_tot> domain[N];
  int<lower=0, upper=1> in_sample[D_tot];
}

transformed data{
  int<lower=0> D = sum(in_sample);
  int<lower=0> D_out = D_tot - D;
}

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
  real u_pred[D_tot];
  int<lower=0, upper=D> in_acc = 0;
  
  for(d in 1:D_tot){
    if(in_sample[d] == 0)
      u_pred[d] = normal_rng(0, sigma_u);
    else{
      in_acc += 1;
      u_pred[d] = u[in_acc];
    }
  }
  
  for(n in 1:N){
    y_pred[n] = exp(student_t_rng(nu, 
                    intercept + X[n] * beta + u_pred[domain[n]], 
                    sigma_e)) - lambda;

  }
}