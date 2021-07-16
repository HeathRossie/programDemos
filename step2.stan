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


transformed parameters {
      // vector[N] log_p;
      real mu[N];
      
      for(i in 1:N)
        mu[i] = i < tau ? beta0 : beta0+beta1;

      
      // log_p = rep_vector(-log(N), N);
      // for (tau in 1:N)
      //   for (n in 1:N) {
      //     mu = n < tau ? beta0 : beta0+beta1;
      //     log_p[i] = log_p[i] + normal_lpdf(d[n] | mu, sigma);
      // }
}

model{
  for(i in 1:N)
    d[i] ~  normal(mu[i], sigma);
  // target += log_sum_exp(log_p);

    // target += normal_lpdf(d[i] | mu[i], sigma);
  
  // prior
  // target += normal_lpdf(beta0 | 0, 100);
  // target += normal_lpdf(beta1 | 0, 100);
  // target += uniform_lpdf(sigma | 0.0001, 100);
  // target += uniform_lpdf(tau | 0, 1);

}

generated quantities{
  real pred[N];
  for(i in 1:N)
    pred[i] = normal_rng(mu[i], sigma);
}
