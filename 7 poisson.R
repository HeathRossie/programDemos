#-----豊田本7.2-----#
# ポアソン分布を用いた推測

library(rstan)
library(ggplot2)
library(dplyr)
x<-c(1,0,0,3,0,0,0,0,0,1)
N<-10

# ながれ星問題
hist(x)

# ながれ星は5分で平均何個見られるか？
# 5分で見られるながれ星のばらつきはどれくらいか？
# 次の5分間でながれ星が2個観測できる確率はどれくらいか？

# 頻度主義なやりかた
mod <- glm(x~1, family=poisson(link=identity))
coef(mod) 
dpois(2, coef(mod))

# べいずなやりかた
stan_code <- '
data{
  int N;
  int x[N];
}

parameters{
  real<lower=0> lambda;
}

model{
  for(i in 1:N)
    x[i] ~ poisson(lambda);
}

generated quantities{
  real<lower=0, upper=1> p;
  real sqrt_lambda;
  
  p <- exp(-lambda) * pow(lambda, 2) * falling_factorial(1,2);
  # falling_factorial(x,n)はx!/n!を返す  sqrt_lambda <- sqrt(lambda);
}
'

dat <- list(x=x,N=N)
fit <- stan(model_code = stan_code, data = dat, iter = 1100, warmup = 100, chain = 1)

d <- extract(fit)
mean(d$p)
mean(d$lambda)

dpois(2,d$lambda) %>% mean
