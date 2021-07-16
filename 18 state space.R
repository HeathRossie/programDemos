set.seed(1234)
setwd("~/OneDrive/R/demo/stan code")

n <- 100
sigma1 <- 2000
sigma2 <- .5
sigma3 <- 5
sigma4 <- 2
x <- 1:n

t <- rnorm(n, sigma1, 5)
beta <- c(.1, rep(0, n-1))
for(i in 2:n) beta[i] <- rnorm(1, beta[i-1], sigma2)
z <- c(5, rep(0, n-1))
for(i in 2:n) z[i] <- rnorm(1, z[i-1], sigma3)

y <- t + z * beta + rnorm(n, 0, sigma4)

plot(y, type = "l")

plot(t, y)
plot(z, y)

library(rstan)
library(ggplot2)
library(dplyr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dat.list <- list(n=n, z=z, y=y)
fit <- stan(
  file = "18 state space.stan",
  data = dat.list,
  iter = 11000,
  warmup = 1000,
  chain = 3
)

res <- extract(fit)
ebeta <- res$beta %>% colMeans 
plot(beta,ebeta)

et <- res$t %>% colMeans 
plot(t,et)

plot(x,y, type = 'l')
lines(x, colMeans(res$t) + ebeta*z, col = "red")

