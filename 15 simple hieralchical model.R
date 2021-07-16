#-----KuboBook 10章-----#
# 階層ベイズ例題

library(rstan)
library(ggplot2)
library(dplyr)

d <- read.csv("http://hosho.ees.hokudai.ac.jp/~kubo/stat/iwanamibook/fig/hbm/data7a.csv")
d %>% head

# 8個中何個の種子が生きていたか、というデータを100サンプル集めてきた
# だけどあまり二項分布っぽい形をしていない
hist(d$y)

d.list <- list(N=nrow(d), y=d$y)

par <- c("beta", "sigma", "postpred")
fit <- stan(
  file =  "15 simple hierarchical model.stan",
  data = d.list,
  iter = 11000,
  warmup = 1000,
  chain = 3,
  pars=par
)

fit %>% traceplot

result.stan <- extract(fit)
result.stan$sigma %>% mean
result.stan$beta %>% mean

result.stan$postpred %>% str
result.stan$postpred %>% colMeans() %>% hist

d$y %>% table %>% plot(.,type = "b")

par(mfrow=c(2,1))
d$y %>% hist
postpred_m <- result.stan$postpred %>% colMeans() %>% floor
ppdm <- as.data.frame(table(postpred_m))
y <- as.data.frame(table(d$y))

ggplot() + 
  geom_line(data=ppdm, aes(x=as.integer(postpred_m), y=Freq), colour="red") +
  geom_point(data=ppdm, aes(x=as.integer(postpred_m), y=Freq), size = 5, colour="red")  +
  geom_line(data=y, aes(x=as.integer(Var1), y=Freq)) +
  geom_point(data=y, aes(x=as.integer(Var1), y=Freq), size = 5) +
  xlim(values=c(0,8)) 
  
  
