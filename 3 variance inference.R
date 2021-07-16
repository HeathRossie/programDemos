#-----豊田本6.1.2-----#
#-----分散に関する推測-----#
#-----工場機器買い替え問題-----#

library(rstan)
library(ggplot2)
library(dplyr)
N<-40
x<-c(
  145.55, 145.41, 144.26, 145.05, 145.84, 145.06, 145.19, 145.30, 144.47,
  144.84, 145.18, 145.00, 144.95, 144.88, 145.25, 145.38, 145.28, 144.66,
  145.26, 144.47, 145.24, 144.29, 145.21, 144.77, 145.51, 144.33, 144.47,
  144.90, 144.76, 145.46, 145.04, 144.98, 145.41, 145.45, 144.83, 144.71,
  144.65, 144.21, 145.10, 145.10)

# 分散が0.1を越えている確率はどれくらいか？
# 分散が0.15を越えた確率が80％以上のとき買い換える
# 買い換えるべきか？

var(x)

stan_code <- '
data{
  int<lower=0> N;
  real<lower=0> x[N];
}

parameters{
  real<lower=0> sigma;
}

transformed parameters{
  real<lower=0> sigmasq;
  
  sigmasq <- pow(sigma, 2);
}

model{
  for(i in 1:N)
    x[i] ~ normal(145, sigma);
}

generated quantities{
  real<lower=0, upper=1> sigma_over;
  real<lower=0, upper=1> sigma_over2;
  
  sigma_over <- step(sigmasq-0.1);
  sigma_over2 <- step(sigmasq-0.15);
}
'

d_list <- list(N=N, x=x)

fit <- stan(model_code = stan_code, data = d_list,
  iter = 1100, warmup = 100, chain = 1)

d <- extract(fit)

sum(d$sigma_over)/1000
sum(d$sigma_over2)/1000

ggplot() + geom_density(aes(x=d$sigmasq))
