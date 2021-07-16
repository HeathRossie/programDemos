### Wiener diffusion process
# see
# https://mc-stan.org/docs/2_18/functions-reference/wiener-first-passage-time-distribution.html
# https://journal.r-project.org/archive/2014-1/vandekerckhove-wabersich.pdf


library(tidyverse)
get_pr = function(){
  t = 1:1000 / 1000
  v = 1
  y = 0
  s = 2
  delta_t = 0.01
  for(i in 1:length(t)){
    y = c(y,
          y[i] + v * delta_t + s * rnorm(1) * sqrt(delta_t)
    )
  }
  
  t = c(0, t)

  return(y)
}

y = replicate(10000, get_pr())

y = reshape2::melt(y)


ggplot(y) + 
  geom_line(aes(x = Var1, y=value, group=as.factor(Var2)), alpha=0.01, colour="gray")



library(RWiener)
dat =  rwiener(n=1000, alpha=1, tau=.3, beta=.5, delta=1)
dat = dat[dat$resp=="upper",]
ggplot(dat) + 
  geom_density(aes(x=q, fill = resp))
  


stancd = "
data{
int N;
real y[N];
}
parameters{
real<lower=0> a;
// real<lower=0, upper=1> b;
real d;
real<lower=0> tau;
}

transformed parameters{
real<lower=0, upper=1> b;
b = 0;

}

model{
y ~ wiener(a, tau, b, d);
}
"

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fit = stan(model_code = stancd, data=list(N=nrow(dat), y=dat$q),
           warmup = 10000, iter = 10500, chain = 4
           )
traceplot(fit)
summary(fit)$summary[,"Rhat"][which(summary(fit)$summary[,"Rhat"] >= 1.1)]
res = rstan::extract(fit)
mean(res$a)
mean(res$b)
mean(res$d)
mean(res$tau)

res$d %>% range


pred =  rwiener(n=1000, alpha=mean(res$a), tau=mean(res$tau), beta=mean(res$b), delta=mean(res$d))

ggplot() + 
  geom_density(data=dat,aes(x=q, fill = resp), alpha=0.4) + 
  geom_density(aes(x=pred[pred$resp=="upper",]$q), alpha=0.4) 





