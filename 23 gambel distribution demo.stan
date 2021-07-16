data{
  int N;
  real ht[N];
  int mon[N];
}

parameters {
	real<lower=0>   mu[12];
	real<lower=0>	sigma[12];
}

model {
  for(i in 1:N){
    ht[i] ~ gumbel(mu[mon[i]], sigma[mon[i]]);  
  }
}

generated quantities{
  real htpred[12];
  
  // 各月の予想最高気温
  for(i in 1:12){
    htpred[i] = gumbel_rng(mu[i],sigma[i]);
  }
  
}
