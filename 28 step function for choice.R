library(ggplot2)
library(rstan)
library(gridExtra)
library(bridgesampling)
library(bayesplot)
rstan_options(auto_write=TRUE)
options(mc.cores = parallel::detectCores())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

N = 1000
# data generation
data_gen = function(N = 1000, type = "step"){
  if(type == "step"){
    tau = 400
    data = rep(-2, N)
    data[400:N] = data[400:N] + 3.5
    data = 1/(1+exp(-data))
    data = rbinom(N, 1, data)
    
  }else{
    
    gamma = 1
    n0 = -3
    # n1 = 0.1
    t = seq(1,N,1)/N
    data = rbinom(N, 1, 1/(1+exp(-(gamma*t+n0))))
  }
  return(data)
}

CI = function(pred, lim = "upper"){
  if(lim == "upper") bound = apply(pred, 2, function(x) quantile(x, .975))
  else bound = apply(pred, 2, function(x) quantile(x, .025))
}

data = data_gen(1000, "sig")
ggplot() + geom_line(aes(x=(1:N)/N, y=data))

sum(data[1:399])/length(data[1:300])
sum(data[400:N])/length(data[400:N])




fit1 = vb(stan_model("step3 binom.stan"), data = list(d=data, N=N, t=(1:N)/N), algorithm="meanfield")
fit1 = stan(file = "step2.stan", data = list(d=data, N=N, t=(1:N)/N),
           iter=1000, warmup = 900, chain=4)
res1 = rstan::extract(fit1)


ggplot() + geom_point(aes(x=(1:N)/N, y=data)) +
  geom_vline(xintercept = mean(res1$tau)/N, lwd = 2, alpha=.5, lty=1) +
  geom_line(aes(x=(1:N)/N, y=apply(res1$pred, 2, mean)), colour="red") 

quantile(res1$tau, prob=c(0.025, 0.975))

ggplot() + geom_line(aes(x=(1:N)/N, y=data)) +
  geom_vline(xintercept = mean(res1$tau)/N, lwd = 2, alpha=.5, lty=1) +
  geom_ribbon(aes(x=(1:N)/N, ymin=CI(exp(res1$mu), "lower"), ymax=CI(exp(res1$mu), "upper")), alpha=.3, fill="red") + 
  geom_line(aes(x=(1:N)/N, y=apply(exp(res1$mu), 2, mean)), colour="red") 

#---------------------------------------------------------------------------------------
# sigmoidと比較する
fit2 = vb(stan_model("logistic binom.stan"), data = list(d=data, N=N, t=1:N), algorithm = "fullrank")
fit2 = stan(file = "logistic.stan", data = list(d=data, N=N, t=(1:N)/N),
           iter=1000, warmup = 900, chain=4)
res2 = rstan::extract(fit2)

summary(fit1)$summary[,"Rhat"][which(summary(fit1)$summary[,"Rhat"] >= 1.1)]
summary(fit2)$summary[,"Rhat"][which(summary(fit2)$summary[,"Rhat"] >= 1.1)]


ggplot() + geom_line(aes(x=(1:N)/N, y=data)) +
  geom_line(aes(x=(1:N)/N, y=apply(res2$mu, 2, mean)), colour="red")

grid.arrange(ggplot() + geom_line(aes(x=(1:N)/N, y=data)) +
               geom_ribbon(aes(x=(1:N)/N, ymin=CI(res1$pred, "lower"), ymax=CI(res1$pred, "upper")), alpha=.3, fill="red") + 
               geom_line(aes(x=(1:N)/N, y=apply(res1$pred, 2, mean)), colour="red"),
             ggplot() + geom_line(aes(x=(1:N)/N, y=data)) +
               geom_ribbon(aes(x=(1:N)/N, ymin=CI(res2$pred, "lower"), ymax=CI(res2$pred, "upper")), alpha=.3, fill="red") + 
               geom_line(aes(x=(1:N)/N, y=apply(res2$pred, 2, mean)), colour="red"),
             ncol=1
)

# bf
model_step = bridge_sampler(fit1, method = "normal", silent = TRUE)
model_sigmoid = bridge_sampler(fit2, method = "normal", silent = TRUE)
BF = bf(model_step, model_sigmoid)
BF
BF2 = bf(model_sigmoid, model_step)
BF2
