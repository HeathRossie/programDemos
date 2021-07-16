// 確率過程の時系列分析
// 時系列が複数のラインあるタイプ

data{
  
  int<lower=0> maxlen;
  int<lower=0> n;
  int t[n]; // number of observation for each line
  real x[maxlen, n]; // explanatory variable
  int<lower=0, upper=1> beg10[maxlen, n]; // beg or no-beg in a certain time-window
  int<lower=0> outcome[maxlen, n]; // response variable, binomial data
  int<lower=0> trial[maxlen, n]; 
  
}

parameters{
  
  real beta; // effect of explanatory variable
  real beta_beg; // effect of beg 
  real level[maxlen, n]; // noise term depending on a t-1 time-point
  real<lower=0.0001> sig1; // variance of system noise
  real<lower=0.0001> sig2; // variance of system noise2
  real gamma[n]; // individual differences
  real<lower=0.0001> sigma_gamma; // variance of individual differences
  
}

transformed parameters{
  
  real<lower=0,upper=1> p[maxlen, n]; // actual probability of binomial distribution
  
  for(h in 1:n){ // loop for id
  for(i in 1:t[n]) // loop for trials
    p[i,h] <- inv_logit(beta * x[i,h] + beta_beg * beg10[i,h] + level[i,h] + gamma[h]);
  }
  
}

model{
  // probalistic level
  for(h in 1:n){
  for(i in 2:t[n]){
    level[i, h] ~ normal(level[i-1, h], sig1);
  }
  }
  
  // initail value of levels
  for(h in 1:n){
    level[1, h] ~ normal(0, 10000);
  }
  
  // 
  for(h in 1:n){
  for(i in 1:t[n])
    outcome[i, h] ~ binomial(trial[i, h], p[i, h]);
  }
  
  // random effects term
  for(i in 1:n) 
    gamma[n] ~ normal(0, sigma_gamma);
    
}



