# install.packages('brms')
library(brms)
library(rstan)
library(ggplot2)
library(gridExtra)

x1 = runif(100, 0, 2*pi)
x2 = runif(100, 0, 2*pi)
y = sin(x1) + sin(x2) + rnorm(100, sd = .1)

ggplot() + geom_point(aes(x=x1, y=x2, colour=y), size=3) + 
  scale_colour_gradientn(colours=c("gray", "orange", "red"))

rstan_options(auto_write=TRUE)
options(mc.cores = parallel::detectCores())

fit = brm(y ~ gp(x1, x2), chain = 4, iter=2000, warmup=1500, data=list(x1=x1,x2=x2,y=y))
summary(fit)

n = 10
xx1 = seq(0, 2*pi, length=n)
xx2 = seq(0, 2*pi, length=n)

res = fitted(fit, data.frame(x1=rep(xx1, n), x2=rep(xx2, each=n)), summary=TRUE)
res = as.data.frame(res)
res$xx1 = rep(xx1, n)
res$xx2 = rep(xx2, each=n)

ggplot() + 
  geom_tile(data=res, aes(x=xx1,y=xx2,fill=Estimate)) + 
  geom_point(aes(x=x1, y=x2), size=5) + 
  geom_point(aes(x=x1, y=x2, colour=y), size=3) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_colour_gradientn(colours=c("gray", "orange", "red")) +
  scale_fill_gradientn(colours=c("gray", "orange", "red"))

