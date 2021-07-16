data{
  int N;
  real d[N];
  real t[N];
}

parameters{
  real<lower=0, upper=t[N]> tau; 
  real beta0;
  real beta1;
  real<lower=0.0001> sigma;
}

transformed parameters{
 real mu[N];
 for(i in 1:N)
  mu[i] = beta0 + beta1 * step(t[i] - tau);
}

model{
  for(i in 1:N)
    target += normal_lpdf(d[i] | mu[i], sigma);
  
  // prior
  target += normal_lpdf(beta0 | 0, 100);
  target += normal_lpdf(beta1 | 0, 100);
  target += uniform_lpdf(sigma | 0.0001, 100);
  target += uniform_lpdf(tau | 0, t[N]);

}

generated quantities{
  real pred[N];
  for(i in 1:N)
    pred[i] = normal_rng(mu[i], sigma);
}
