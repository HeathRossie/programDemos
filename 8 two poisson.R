#-----豊田本7.1.2-----#
# 2つのポアソン分布を用いた推測
# ウミガメ問題


library(rstan)
library(ggplot2)
library(dplyr)

x1<-c(0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 
  1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ,0 ,1)
x2<-c(0, 3, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 
  3, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 2)
N<-30 

sum(x1)/N
sum(x2)/N

stan_code <- '
data{
  int<lower=0> N;
  int<lower=0> x1[N];
  int<lower=0> x2[N];
}

parameters{
  real<lower=0> lamda1;
  real<lower=0> lamda2;
}

model{
  x1 ~ poisson(lamda1);
  x2 ~ poisson(lamda2);
}

generated quantities{
  real delta;
  real<lower=0,upper=1> p_delta;
  delta <- lamda2 - lamda1;
  p_delta <- step(delta);
}
'

dat <- list(x1=x1, x2=x2, N=N)
fit <- stan(model_code = stan_code, data = dat, iter = 1100, warmup = 100, chain = 1)

d <- extract(fit)

d$delta %>% mean
sum(d$p_delta)/1000
