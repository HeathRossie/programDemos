data{
  int<lower=1> N;
  int<lower=1> n;
  int ind1[N];
  int ind2[N];
  int<lower=0, upper=1> outcome[N];
}

parameters{
  real gamma[n];
  real<lower=0> sigma;
}

transformed parameters{
  real rhp_diff[N];
  real rhp_diff_logit[N];
  
  for(i in 1:N)
  rhp_diff[i] <- gamma[ind1[i]] - gamma[ind2[i]];
}

model{
  for(i in 1:N)
 outcome[i] ~ binomial(1, inv_logit(rhp_diff[i]));
 
 for(i in 1:n)
  gamma[i] ~ normal(0, sigma);
}

