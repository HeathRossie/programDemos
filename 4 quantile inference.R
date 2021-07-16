#-----豊田本6.1.3-----#
# 分位に関する推測
# 代表選考ボーダーライン問題

library(rstan)
library(ggplot2)
library(dplyr)

N<-20  
x<-c(775, 779, 799, 794, 770, 790, 775,  
  778, 808, 802, 776, 775, 799, 787,
  825, 785, 775, 762, 782, 788)

# 選考に残るは上位25%
# mean 905, SD 10 の選手が選考を進む確率はどれくらいか？

mean(x)
quantile(x, .75)

stan_code <-'
data{
  int N;
  int x[N];
}

parameters{
  real mu;
  real sigma;
}

model{
  for(n in 1:N)
    x[n] ~ normal(mu, sigma);
}

generated quantities{
  real q_3rd;
  real<lower = 0, upper = 1> p_over;
  

  q_3rd <- mu + 0.675 * sigma;
  p_over <- 1-normal_cdf(q_3rd, 805, 10);
}
'

d_list <- list(N=N, x=x)
fit <- stan(model_code = stan_code, data = d_list, iter = 1100, warmup = 100)

d <- extract(fit)
d$q_3rd %>% mean
d$p_over %>% mean
d$p_over %>% quantile(., .025)
d$p_over %>% quantile(., .975)
