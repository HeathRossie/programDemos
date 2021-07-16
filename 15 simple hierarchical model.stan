data{
  int N;
  int y[N];
}

parameters{
  real beta; // 全個体共通生存率
  real r[N]; // 個体差パラメータ
  real<lower=0> sigma;
}

transformed parameters{
  real q[N];
  for(i in 1:N)
    q[i] <- inv_logit(beta + r[i]);
}

model{
  for(i in 1:N)
    y[i] ~ binomial(8, q[i]);
  
  for(i in 1:N)
    r[i] ~ normal(0, sigma);
  
  // 事前分布を設定する
  beta ~ normal(0, 100);
  sigma ~ uniform(0, 1.0e+4);
}

generated quantities{
  int postpred[N];
  for(i in 1:N)
    postpred[i] <- binomial_rng(8, q[i]);
}