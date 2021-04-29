  vector[N] log_lik;
  for(n in 1:N)
    log_lik[n] = normal_lpdf(y[n] | mu[n], sigma);