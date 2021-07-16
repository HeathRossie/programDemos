#-----みどりぼん11章-----#
# 空間構造のある階層ベイズモデル

library(rstan)
library(ggplot2)
library(dplyr)
load("/Users/matsuidai/Dropbox/R/demo/stan code/Y.RData")
Y

# こういうデータ
ggplot(data.frame(X=seq(1:length(Y)), Y=Y), aes(x=X, y=Y)) +
  geom_point(size=4) + geom_smooth(lty = 2)

# 全体が平均λのポアソン分布から生成されているとすると
mean(Y)
glm(Y~1, family=poisson) %>% coef %>% exp

# ただし過分散が生じている
sd(Y)^2

#-----stanで推定してみる-----#
setwd("~/Dropbox/R/demo/stan code")
dat <- list(N=length(Y), Y=Y)

fit <- stan(
  file = "17 stancode.stan",
  data = dat,
  iter = 11000,
  warmup = 1000,
  chain = 3
)

fit

stan.result <- extract(fit)

quantile2 <- function(x){
  quan <- quantile(x, c(.025,.975))
}

CI <- apply(stan.result$lambda, 2, quantile2)

ggplot() +
  geom_ribbon(aes(x=seq(1:length(Y)), ymin=CI[1,], ymax=CI[2,]), fill="red", alpha=.2) +
  geom_line(aes(x=seq(1:length(Y)), y=colMeans(stan.result$lambda)), colour="red", lty=2) +
  geom_point(data=data.frame(X=seq(1:length(Y)), Y=Y), aes(x=X, y=Y), size=4) 

mean(stan.result$gamma) %>% exp
stan.result$gamma %>% colMeans %>% hist
