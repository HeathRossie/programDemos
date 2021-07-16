#-----豊田本7.2-----#
# 指数分布を持ちいた推測
# ハンバーグ注文間隔問題

library(rstan)
library(ggplot2)
library(dplyr)

x<-c(5,1,18,5,1,8,8,2,14,12)
N<-10

# 平均的な注文間隔は？
# 残り5分で注文してくる確率は？
# それが30%を越える確率はどれくらいか？

hist(x)

stan_code <- '
data{
  int<lower=0> N;
  real<lower=0> x[N];
}

parameters{
  real<lower=0> lambda ;
}

model{
	x ~ exponential(lambda);
}

generated quantities{
  real<lower=0> mu;
  real<lower=0, upper=1> p;
  real<lower=0, upper=1> p_over;
  
  mu <- 1/lambda;
  p <- exponential_cdf(5, lambda);
  p_over <- step(p - 0.3);
}
'

dat <- list(x=x, N=N)
fit <- stan(model_code = stan_code, data = dat, iter = 1100, warmup = 100, chain = 1)
d <- extract(fit)
d$p %>% mean
sum(d$p_over)/1000
d$lambda %>% mean
