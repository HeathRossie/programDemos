// 確率過程の時系列分析

data{
  int t; // number of observation
  real x[t]; // explanatory variable
  int<lower=0, upper=1> beg10[t]; // beg or no-beg in a certain time-window
  int<lower=0> outcome[t]; // response variable, binomial data
  int<lower=0> trial[t]; 
}

parameters{
  real beta; // effect of explanatory variable
  real beta_beg; // effect of beg 
  real level[t]; // noise term depending on a t-1 time-point
  real<lower=0> sig1; // variance of system noise
  real<lower=0> sig2; // variance of system noise2
}

transformed parameters{
  real<lower=0,upper=1> p[t]; // actual probability of binomial distribution
  for(i in 1:t)
    p[i] <- inv_logit(beta * x[i] + beta_beg * beg10[i] + level[i]);
}

model{
  for(i in 2:t){
    level[i] ~ normal(level[i-1], sig1);
  }
  
  for(i in 1:t)
    outcome[i] ~ binomial(trial[i], p[i]);
  
}



