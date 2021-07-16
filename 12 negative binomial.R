#-----豊田本7.5-----#
# 負の二項分布を用いた推測
# エントリーシート問題
# 20社だした結果が以下だった
# 5通出したときに2通以上通る確率
# 2通以上通したければ何通出せばいいのか

N<-20
x <- c(0,0,0,1,0,1,1,0,0,0,0,0,0,0,1,0,0,0,1,0)

data{
  int<lower=0> N;
  int<lower=0> x;
}

parameters{
  real<lower=0,upper=1> theta;
}

model{
  x ~ bernoulli(theta);
}

generated quantities{
  real<lower=0> p;
  real<lower=0> nu;
  
  p <- 1*pow(theta,5)
}