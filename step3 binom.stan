// http://nowave.it/pages/bayesian-changepoint-detection-with-r-and-stan.html

data {
    int<lower=1> N;
    int<lower=0, upper=1> d[N]; 
}

// stan operates on log scale
transformed data {
    real log_unif;
    log_unif = log(N);
}

parameters {
    real q1;
    real q2;
}

// Marginalize out tau and
// calculate log_p(D | mu1, sd1, mu2, sd2)
// TODO: we can make this linear via dynamic programming
transformed parameters {
      vector[N] log_p;
      real q;
      log_p = rep_vector(log_unif, N);
      for (tau in 1:N)
        for (i in 1:N) {
          q = i < tau ? q1 : q2;
          log_p[tau] = log_p[tau] + binomial_logit_lpmf(d[i] | 1, q);
      }
}

    
model {
  target += normal_lpdf(q1 | 0, 100);
  target += normal_lpdf(q2 | 0, 100);
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
      pred[i] = binomial_rng(1, inv_logit(q1));
      mu[i] = inv_logit(q1);
    } 
    else{
      pred[i] = binomial_rng(1, inv_logit(q2));
      mu[i] = inv_logit(q2);
    } 
  }
    
}
