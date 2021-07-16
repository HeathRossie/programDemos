#-----みどりぼん6章-----#
# ロジスティック回帰

library(rstan)
library(ggplot2)
library(d@lyr)
d <- read.csv("http://hosho.ees.hokudai.ac.jp/~kubo/stat/2016/Fig/binomial/data4a.csv")
d %>% head 
d$fd <- 0
d[d$f == "T",]$fd <- 1

d_list <- list(N=nrow(d), y=d$y, x=d$x, fd=d$fd)

stancode <- '
data{
  int<lower=0> N;
  real<lower=0> x[N];
  int<lower=0, upper=8> y[N];
  int<lower=0> fd[N];
}

parameters{
  real beta1;
  real beta2;
  real beta3;
  real beta4;
}

model{
for(i in 1:N)
  y[i] ~ binomial(8, inv_logit(beta1 + beta2 * x[i] + beta3 * fd[i] + beta4 * x[i] * fd[i]));
}

generated quantities{
  real<lower=0, upper=1> delta;
  delta <- step(beta3);
}
'

fit <- stan(model_code = stancode, data = d_list, iter = 11000, warmup = 1000,
  chain = 3)
res.stan <- extract(fit)
traceplot(fit)
p1 <- ggplot() + geom_density(aes(x=res.stan$beta1))
p2 <- ggplot() + geom_density(aes(x=res.stan$beta2))
p3 <- ggplot() + geom_density(aes(x=res.stan$beta3))
p4 <- ggplot() + geom_density(aes(x=res.stan$beta4))

library(gridExtra)
grid.arrange(p1,p2,p3,p4,ncol=2,nrow=2)
sum(res.stan$delta)/length(res.stan$delta)

c(res.stan$beta1 %>% mean,
  res.stan$beta2 %>% mean,
  res.stan$beta3 %>% mean,
  res.stan$beta4 %>% mean)
glm(cbind(y, 8-y) ~ x * fd, data = d, family = binomial) %>% coef

