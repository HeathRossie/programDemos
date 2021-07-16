library(ggplot2)
library(dplyr)
library(rstan)
p <- .7
lam <- 3
N <- 1000
Y <- rbinom(N, size=1, prob=p) * rpois(N, lam)

ggplot() + 
  geom_histogram(aes(x=Y))

code <- '
data{
  int N;
  int Y[N];
}

parameters{
  real<lower=0,upper=1> p;
  real lambda;
}

model{
  for(i in 1:N){
    if(Y[i]==0){
      target += log_sum_exp(bernoulli_lpmf(0|p),
                            bernoulli_lpmf(1|p) + poisson_lpmf(0|exp(lambda)));
    }else{
      target += bernoulli_lpmf(1|p) + poisson_lpmf(Y[i]|exp(lambda));
    }
  }
}

generated quantities{
  int Y_pred[N];
  for(i in 1:N){
    Y_pred[i] = bernoulli_rng(p) * poisson_log_rng(lambda);
  }
}
'

fit <- stan(model_code = code, data=list(N=length(Y), Y=Y), 
            iter=2250, warmup=2000, chain=4)
summary(fit)$summary[,"Rhat"][which(summary(fit)$summary[,"Rhat"] >= 1.1)]
res <- rstan::extract(fit)
mean(res$p)
mean(exp(res$lambda))

M <- matrix(0, nrow=1000, ncol=1000)
table(res$Y_pred[,1])

tmp <- apply(res$Y_pred, 1, function(x) table(factor(x,levels=0:max(res$Y_pred))))

apply(tmp, 1, max)

ggplot() + 
  geom_histogram(aes(x=Y), position = "identity", , binwidth = 1, colour="black", fill="gray80") +
  geom_ribbon(aes(x=0:max(res$Y_pred),ymin=apply(tmp, 1, function(x)quantile(x,.025)), ymax=apply(tmp, 1, function(x)quantile(x,.975))), fill="red", alpha=.5) + 
  geom_line(aes(x=0:max(res$Y_pred),y=apply(tmp, 1, mean)), colour="red", alpha=1) +
  geom_point(aes(x=0:max(res$Y_pred),y=apply(tmp, 1, mean)), colour="red", alpha=1)

 anastr(tmp)

