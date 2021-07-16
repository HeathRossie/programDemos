data{
  int<lower=0> n;
  int<lower=0> t[n];
  real y[n];
}

parameters{
  real beta;
  real mu[n];
  real<lower=0> sigma1;
  real<lower=0> sigma2;
}

model{
  for(i in 2:n)
  mu[i] ~ normal(mu[i-1], sigma1);
  
  for(i in 2:n)
  y[i] ~ normal(mu[i-1] + beta*t[i], sigma2);
}


