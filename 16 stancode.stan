// KuboBook 10章のstan code

data{
  int<lower=0> N_sample; // サンプルサイズ
  int<lower=0> N_pot; // potの数
  int<lower=0> N_sigma;
  int<lower=0> y[N_sample]; // 観測種子数
  int<lower=0> f[N_sample]; // 肥料の有無
  int<lower=0> Pot[N_sample]; // pot
  
}

parameters{
  real beta1; // 全個体共通切片
  real beta2; // 肥料の効果
  real r[N_sample]; // 個体差パラメータ
  real rp[N_pot]; // 場所差パラメータ
  real<lower=0> sigma[N_sigma]; // 分散パラメータ
}

transformed parameters{
  real<lower=0> lambda[N_sample];
  for(i in 1:N_sample)
    lambda[i] <- exp(beta1 + beta2 * f[i] + r[i] + rp[Pot[i]]);
}

model{
  for(i in 1:N_sample)
    y[i] ~ poisson(lambda[i]);
    
  // prior distribution
  beta1 ~ normal(0, 100);
  beta2 ~ normal(0, 100);
  r ~ normal(0, sigma[1]);
  rp ~ normal(0, sigma[2]);
  for(k in 1:N_sigma)
    sigma[k] ~ uniform(0, 1.0e+4);
}








