#----2項選択の時系列分析-----#
# ローカルレベル + 説明変数1つ
# 1つのラインのみの時系列

library(ggplot2)
library(rstan)
setwd("~/Dropbox/R/demo/stan code")

#---ローデータの作成----#
# データは300点の系列/試行数はテキトーにランダムにしてみた・・・
t <- 300 
z <- c(rep(NA, t))
trial <- rpois(300, 10)
# 説明変数xに対するパラメータの真の値
beta <- -.05
x <- rnorm(t)

# 初期値の設定
x[1] <- -30
z[1] <- beta * x[1] 

# データの生成
for(i in 2:t) {
  z[i] <- beta * x[i] + rnorm(1, mean=z[i-1], sd=.3)
}

# zのlogisticが確率pとなる
p <- 1/(1+exp(-z))
plot(p, ylim=c(0,1))
tt <- 1:t; lines(predict(loess(p ~ tt,span=.2)),lwd=1, col="red")

# 確率pに応じたoutcomeが1/0で与えられる
d <- rbinom(t, p = p, size=trial)

#-----stan sampling 実行-----#

d.list <- list(d=d,t=length(d), x=x, trial = trial)
fit <- stan(file = "20 binomial state space demo.stan",
            iter = 33000,
            warmup = 3000,
            chains = 5,
            data = d.list)

res <- extract(fit)

points(colMeans(res$p), type = "l", col = "blue")
mean(res$beta)
beta

ggplot() +
  geom_density(aes(x=res$beta))

ggplot() +
  geom_point(aes(x=colMeans(res$p),y=p)) +
  geom_abline(slope=1)


