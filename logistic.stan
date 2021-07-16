data{
  int N;
  real d[N];
  real t[N];
}

parameters{
  real n0;
  real n1;
  real gamma;
  real K;
  real<lower=0.0001> sigma;
}

transformed parameters{
  real mu[N];
  for(i in 1:N)
    mu[i] = K/(1+exp(-(gamma*t[i]+n0))) + n1;
}

model{
  for(i in 1:N)
    target += normal_lpdf(d[i] | mu[i], sigma);
  
  // prior
  target += normal_lpdf(n0 | 0, 100);
  target += normal_lpdf(n1 | 0, 100);
  target += normal_lpdf(gamma | 0, 100);
  target += normal_lpdf(K | 0, 100);
  target += uniform_lpdf(sigma | 0, 100);
}

generated quantities{
  real pred[N];
  for(i in 1:N)
    pred[i] = normal_rng(mu[i], sigma);
}
