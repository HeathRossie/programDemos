// http://nowave.it/pages/bayesian-changepoint-detection-with-r-and-stan.html
// using change point detection


data {
    int<lower=1> N;
    int<lower=0> d[N]; 
}

// stan operates on log scale
transformed data {
    real log_unif;
    log_unif = log(N);
}

parameters {
    real lambda1;
    real lambda2;
}

// Marginalize out tau and
// calculate log_p(D | mu1, sd1, mu2, sd2)
// TODO: we can make this linear via dynamic programming
transformed parameters {
      vector[N] log_p;
      real lambda;
      log_p = rep_vector(log_unif, N);
      for (tau in 1:N)
        for (i in 1:N) {
          lambda = i < tau ? lambda1 : lambda2;
          log_p[tau] = log_p[tau] + poisson_log_lpmf(d[i] | lambda);
      }
}

    
model {
  target += normal_lpdf(lambda1 | 0, 100);
  target += normal_lpdf(lambda2 | 0, 100);
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
      pred[i] = poisson_log_rng(lambda1);
      mu[i] = lambda1;
    } 
    else{
      pred[i] = poisson_log_rng(lambda2);
      mu[i] = lambda2;
    } 
  }
    
}
