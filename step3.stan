# http://nowave.it/pages/bayesian-changepoint-detection-with-r-and-stan.html

data {
    int<lower=1> N;
    real d[N]; 
}

// stan operates on log scale
transformed data {
    real log_unif;
    log_unif = log(N);
}

parameters {
    real mu1;
    real mu2;

    real sigma;
}

// Marginalize out tau and
// calculate log_p(D | mu1, sd1, mu2, sd2)
// TODO: we can make this linear via dynamic programming
transformed parameters {
      vector[N] log_p;
      real mu_;
      log_p = rep_vector(log_unif, N);
      for (tau in 1:N)
        for (i in 1:N) {
          mu_ = i < tau ? mu1 : mu2;
          log_p[tau] = log_p[tau] + normal_lpdf(d[i] | mu_, sigma);
      }
}

    
model {
  target += normal_lpdf(mu1 | 0, 100);
  target += normal_lpdf(mu2 | 0, 100);
  target += uniform_lpdf(sigma | 0, 100);
  target += log_sum_exp(log_p);
} 

//Draw the discrete parameter tau. This is highly inefficient
generated quantities {
  int<lower=0,upper=N> tau;
  real pred[N];
  real mu[N];
  
  tau = categorical_rng(softmax(log_p));
  for(i in 1:N){
    if(i < tau){
      pred[i] = normal_rng(mu1, sigma);
      mu[i] = mu1;
    } 
    else{
      pred[i] = normal_rng(mu2, sigma);
      mu[i] = mu2;
    } 
  }
    
}
