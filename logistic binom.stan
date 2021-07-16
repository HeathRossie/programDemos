data{
  int N;
  int<lower=0, upper=1> d[N];
  real t[N];
}

parameters{
  real n0;
  real gamma;
}

transformed parameters{
  real q[N];
  for(i in 1:N)
    q[i] = inv_logit(gamma * t[i] + n0);
}

model{
  for(i in 1:N)
    target += binomial_lpmf(d[i] | 1, q[i]);
  
  // prior
  target += normal_lpdf(n0 | 0, 100);
  target += normal_lpdf(gamma | 0, 100);
}

generated quantities{
  real pred[N];
  real mu[N];
  for(i in 1:N){
    pred[i] = binomial_rng(1, mu[i]);
    mu[i] = q[i];
  }
    
}
