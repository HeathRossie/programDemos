// 空間構造のある階層ベイズモデル

data{
  int<lower=0> N;
  int<lower=0> Y[N];
}

parameters{
  real gamma[N]; // local parameter
  real<lower=0> sigma; 
}

transformed parameters{
  real<lower=0> lambda[N];
  for(i in 1:N){
    lambda[i] <- exp(gamma[i]); // λはlogリンク線形予測子とつながる
  }
}

model{
  // データは平均λのポアソン分布から生成される
  Y ~ poisson(lambda);
  
  // 個体差は近傍データを平均とする正規分布から生成される 
  for(i in 2:N){
   gamma[i] ~ normal(gamma[i-1], sigma);
  }
  // prior
  sigma ~ uniform(0, 1.0e+4);
}





