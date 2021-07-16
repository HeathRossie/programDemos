#----- stan 練習 1 -----#
#-----線形回帰------#

library(rstan)
library(ggplot2)

x <- runif(100, 1, 100)
y <- x * 2/3 + 220 + rnorm(length(x), 0, 10)

ggplot() + geom_point(aes(x=x,y=y))

stan_code <- '

data{
  int<lower=1> N;
  real x[N];
  real y[N];
  
}

parameters{
  real beta1;
  real beta2;
  real<lower=0> sigma;
}

transformed parameters{
  real yhat[N];
  
  for(i in 1:N)
    yhat[i] <- beta1 + beta2 * x[i];
}


model{
  for(i in 1:N)
    y[i] ~ normal(yhat[i], sigma);
}
'

dat_list <- list(x = x, y = y, N = length(x))

fit <- stan(model_code = stan_code, data = dat_list,
  iter = 110, warmup = 10, chain = 1)

d <- extract(fit)

traceplot(fit)

library(gridExtra)
p1 <- ggplot() + geom_density(aes(x=d$beta1))
p2 <- ggplot() + geom_density(aes(x=d$beta2))
grid.arrange(p1, p2, ncol = 2)


pred <- NULL
for(i in 1:10000){
  for(j in c(min(x),max(x))){
    pred <- c(pred, d$beta1[i] + d$beta2[i] * j)
  }
}

predc <- data.frame(pred = pred, iter = rep(1:10000, each = 2),
  x = rep(c(min(x),max(x)), 10000))

ggplot() + 
  geom_line(data = predc, aes(x=x, y=pred, group=iter), colour = "pink", lwd = 1, alpha=.004) +
  geom_point(aes(x=x,y=y)) +
  geom_abline(aes(intercept=mean(d$beta1), slope=mean(d$beta2)), colour = "red", lwd = 1.3)







