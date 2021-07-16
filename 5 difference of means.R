#-----豊田本6.1.4-----#
# 独立な2群の平均値差に関する推測
library(rstan)
library(ggplot2)
library(dplyr)

N1 <- 20
N2 <- 20
x1 <- c(30.86,29.75,31.55,32.29,29.90,31.71,31.35,29.03,30.37,31.55,
  29.26,32.29,29.90,30.18,30.72,32.28,30.72,29.90,31.55,31.55)
x2 <- c(31.36,33.34,33.16,31.36,36.19,29.80,31.11,35.23,31.36,31.27,
  31.63,31.63,32.00,31.11,31.63,31.36,31.81,31.63,29.21,33.37)

# t-testは有意
t.test(x1,x2, paired = FALSE)

stan_code <- '
data{
  int N1;
  int N2;
  real x1[N1];
  real x2[N2];
}

parameters{
  real<lower=0> mu1;
  real<lower=0> mu2;
  real<lower=0> sigma1;
  real<lower=0> sigma2;
}

model{
  for(i in 1:N1)
    x1[i] ~ normal(mu1, sigma1);
  
  for(i in 1:N2)
    x2[i] ~ normal(mu2, sigma2);
}

generated quantities{
  real diff;
  real diff_over;
  
  diff <- mu1 - mu2;
  diff_over <- step(abs(diff) - 1);
}
'

d_list <- list(N1 = N1, N2 = N2, x1 = x1, x2 = x2)

fit <- stan(model_code = stan_code, data = d_list, iter = 1100, warmup = 100)

d <- extract(fit)

mean(d$diff)
quantile(d$diff, c(0.025, 0.975))
sum(d$diff_over)/1000

sum(d$diff_over)/length(d$diff_over)

ggplot() + geom_histogram(aes(x=d$diff)) +
  geom_vline(xintercept=0, lty=2, colour="red")

