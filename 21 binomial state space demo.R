#----2項選択の時系列分析-----#
# ローカルレベル + 説明変数1つ + 1/0の干渉変数1つ
# 1つのラインのみの時系列

library(ggplot2)
library(rstan)
setwd("~/Dropbox/R/demo/stan code")

#---ローデータの作成----#
# カラスがあるt時点で攻撃が受けるか受けないかの確率のモデルを考えたい
# 10秒以内にbeggingをしたか否かで被攻撃の確率を下げるかをモデル化する
# 共変量として1つテキトーに変数を仮定

# データは300点の系列
range <- 5
t <- 300 
z <- c(rep(NA, t))
trial <- rep(1, t)
Prob_beg <- .05
beg <- sample(c(1,0), t, prob=c(Prob_beg, 1-Prob_beg), replace = TRUE)
beg10 <- NULL
for(i in (range+1):length(beg)){
  beg10 <- c(beg10, sum(sum(beg[(i-range):(i-1)]) > 0))
}


# 説明変数xに対するパラメータの真の値
beta <- 4
beta_beg <- -0.8

x <- rnorm(t, mean = 0, sd = .1)

# 初期値の決定
beta0 <- runif(1,-1,1)
z[1] <- beta0 + beta * x[1]

# データの生成
for(i in 2:range){
  beta0[i] <- rnorm(1,mean=beta0[i-1],sd=.03)
  z[i] <- beta * x[i] + rnorm(1, mean=beta0[i], sd=.03)
}
for(i in (range+1):length(x)) {
  beta0[i] <- rnorm(1,mean=beta0[i-1],sd=.03)
  z[i] <- beta * x[i] + beta_beg * beg10[i-range] + rnorm(1, mean=beta0[i], sd=.03)
}

# zのlogisticが確率pとなる
p <- 1/(1+exp(-z))    

d <- data.frame(z=z,p=p,beg=beg,x=x, trial=trial)[-(1:range),]
d$beg10 <- beg10

plot(d$p, ylim=c(0,1), col=d$beg10+1)
tt <- (range+1):t; lines(predict(loess(d$p ~ tt,span=.2)),lwd=1, col="red")

# 確率pに応じたoutcomeが1/0で与えられる
d$outcome <- rbinom(nrow(d), p = d$p, size=d$trial)

#-----stan sampling 実行-----#

d.list <- list(outcome=d$outcome,t=nrow(d), x=d$x, trial = d$trial, beg10 = d$beg10)
fit <- stan(file = "21 binomial state space demo.stan",
            iter = 50000,
            warmup = 25000,
            chains = 1,
            data = d.list)

res <- extract(fit)

plot(d$p, ylim=c(0,1), col=d$beg10+1)
points(colMeans(res$p), type = "l", col = "blue")
beta
mean(res$beta)
quantile(res$beta, c(.025, .975))
beta_beg
mean(res$beta_beg)
quantile(res$beta, c(.025, .975))

ggplot() +
  geom_density(aes(x=res$beta))

ggplot() +
  geom_density(aes(x=res$beta_beg))

ggplot() +
  geom_point(aes(x=(range+1):t ,y=d$p)) +
  geom_line(aes(x=(range+1):t ,y=colMeans(res$p))) 
  
ggplot() +
  geom_point(aes(x=colMeans(res$p),y=d$p)) +
  geom_abline(slope=1)

cor.test(colMeans(res$p), d$p)

