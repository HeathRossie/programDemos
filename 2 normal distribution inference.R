#-----正規分布に関する推測-----#
#-----豊田本の6.1.1-----#
#-----04 March 2016-----#

# カタログ刷新問題
# 従来は平均購入額が2500円だった
# 今回20人にモニターしてみたデータがが以下

# 従来よりも購入額は上がったと言えるか？
# 平均購入額が3000円を超えた確率が70%以上なら変更するつもり
# 2500円以上の効果量が0.8を超える確率はいくらか？

N<-20
x<-c(3060, 2840, 1780, 3280, 3550, 2450, 2200,
  3070, 2100, 4100, 3630, 3060, 3280, 1870,
  2980, 3120, 2150, 3830, 4300, 1880)

library(rstan)
library(dplyr)
library(ggplot2)

x %>% length
x %>% mean
x %>% hist

# one-sample t-test は有意
t.test(x, mu = 2500)

# stan で推定

stan_code <- '
data{
  int<lower=1> N;
  int<lower=0> x[N];
}

parameters{
  real mu;
  real<lower=0> sigma;
}

model{
  for(i in 1:N)
    x[i] ~ normal(mu, sigma);
}

generated quantities{
  real<lower=0,upper=1> mu_over;
  real<lower=0,upper=1> mu_over2;
  real es;
  real<lower=0,upper=1> es_over;
  
  mu_over <- step(mu-2500);
  mu_over2 <- step(mu-3000);
  es <- (mu-2500)/sigma;
  es_over <- step(es-0.8);
}
'

d_list <- list(x=x,N=N)
fit <- stan(model_code = stan_code, data = d_list,
  iter = 1100, warmup = 100, chain = 1)

d <- extract(fit)
d %>% str

ggplot() + geom_density(aes(x=d$mu))
mean(d$mu)
quantile(d$mu, c(.025, .975))

sum(d$mu_over)/1000
sum(d$mu_over2)/1000

d$es %>% mean
sum(d$es_over)/1000

