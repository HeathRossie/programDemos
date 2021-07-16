#-----豊田本6.2.2-----#
# 対応のある2群の平均値差に関する推測

library(rstan)
library(ggplot2)
library(dplyr)

N <- 20
x <- structure(
  .Data = c(6,11,10,13,17,10,10,7,9,1,14,7,7,11,12,12,14,12,7,13,
    7,11,14,13,16,12,8,15,12,3,17,11,9,11,14,12,11,15,11,17),
  .Dim=c(20,2))

stan_code <- '

data{
  int<lower=0> N;
  vector[2] x[N];
}

parameters{
  vector[2] mu;
  vector<lower=0>[2] sigma;
  real<lower=-1, upper=1> rho;
}

transformed parameters{
  matrix[2,2] Sigma;
  
  Sigma[1,1] <- pow(sigma[1],2);
  Sigma[2,2] <- pow(sigma[2],2);
  Sigma[1,2] <- sigma[1] * sigma[2] * rho;
  Sigma[2,1] <- sigma[1] * sigma[2] * rho;
}

model{
  for(i in 1:N)
    x[i] ~ multi_normal(mu, Sigma);
}

generated quantities{
  real delta;
  real delta_over;
  
  delta <- mu[2] - mu[1];
  delta_over <- step(delta);
}
'

d_list <- list(x=x, N=N)
fit <- stan(model_code = stan_code, data = d_list, iter = 1100, warmup = 100)
d<-extract(fit)
quantile(d$delta, c(.025,.975))
mean(d$delta)
sum(d$delta_over)/length(d$delta_over)
