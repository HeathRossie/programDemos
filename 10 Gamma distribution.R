#-----豊田本7.3-----#
# 流れ星問題2

# 50分観測して5分ごとにこれだけの流れ星を見つけた
N<-10
x<-c( 1 ,0 ,0 ,3 ,0 ,0, 0, 0 ,0 ,1)

# あと3つ見つけるには何分待てばよいだろうか

library(rstan)
library(ggplot2)
library(dplyr)

hist(x)
plot(x, type="b")

stan_code <- '
data{
  int<lower=0> N;
  int<lower=0> x[N];
}

parameters{
  real <lower=0.000001> lambda;
}

model{
  x ~ poisson(lambda);
}

generated quantities{
  real<lower=0.000001> eta;
  
  eta <- 5*3/lambda;
}
'

dat <- list(x=x, N=N)
fit <- stan(model_code = stan_code, data = dat, iter = 1100, warmup = 100, chains = 1)
d <- extract(fit)
d$eta %>% mean
