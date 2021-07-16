// 確率過程の時系列分析

data{
  int t; // number of observation
  real x[t]; // explanatory variable
  int<lower=0> d[t]; // response variable, binomial data
  int<lower=0> trial[t];
}

parameters{
  real beta; // effect of explanatory variable
  real level[t];
  real<lower=0> sig1; // variance of system noise
  real<lower=0> sig2; // variance of system noise2
}

transformed parameters{
  real<lower=0,upper=1> p[t]; // actual probability of binomial distribution
  for(i in 1:t)
    p[i] <- inv_logit(beta * x[i] + level[i]);
}

model{
  for(i in 2:t){
    level[i] ~ normal(level[i-1], sig1);
  }
    
  for(i in 1:t)
    d[i] ~ binomial(trial[i], p[i]);
  
}



