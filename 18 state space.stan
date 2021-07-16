data{
  int n;
  real<lower=0> y[n];
  real z[n];
}

parameters{
  real t[n];
  real beta[n];
  real<lower=0> sigma1;
  real<lower=0> sigma2;
  real<lower=0> sigma4;
}

model{
  for(i in 2:n)
  t[i] ~ normal(t[i-1], sigma1);
  
  for(i in 2:n)
  beta[i] ~ normal(beta[i-1], sigma2);
  
  for(i in 1:n)
  y[i] ~ normal(t[i] + z[i] * beta[i], sigma4);

}
