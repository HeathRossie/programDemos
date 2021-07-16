#-----みどりぼん3賞-----#
# ポアソンGLM

library(rstan)
library(ggplot2)
library(dplyr)
d <- read.csv("http://hosho.ees.hokudai.ac.jp/~kubo/stat/iwanamibook/fig/poisson/data3a.csv")
d %>% head
d$fd <- 0
d[d$f=="T",]$fd <- 1

stan_code <- '
data{
  int N;
  int y[N];
  real x[N];
  int fd[N];
}

parameters{
  real beta1;
  real beta2;
  real beta3;
  real beta4;
}

transformed parameters{
  real lambda[N];
  for(i in 1:N)
    lambda[i] <- exp(beta1 + beta2 * x[i] + beta3 * fd[i] + beta4 * x[i] * fd[i]);
}

model{
  y ~ poisson(lambda);
}
'
dat <- list(N=nrow(d), x=d$x, y=d$y, fd=d$fd)
fit <- stan(model_code = stan_code, data = dat)
dstan <- extract(fit)

glm(y ~ x*fd, data=d, family=poisson) %>% coef
glm(y ~ x*f, data=d, family=poisson) %>% coef
dstan$beta1 %>% mean
dstan$beta2 %>% mean
dstan$beta3 %>% mean
dstan$beta4 %>% mean
  