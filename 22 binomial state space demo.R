#----2項選択の時系列分析-----#
# ローカルレベル + 説明変数1つ + 1/0の干渉変数1つ
# 系列が複数

library(ggplot2)
library(rstan)
library(shinystan)
setwd("~/Dropbox/R/demo/demo/utility")
source("data shaping unequal length mat.R")
setwd("~/Dropbox/R/demo/stan code")
#---ローデータの作成----#
# カラスがあるt時点で攻撃が受けるか受けないかの確率のモデルを考えたい
# 10秒以内にbeggingをしたか否かで被攻撃の確率を下げるかをモデル化する
# 共変量として1つテキトーに変数を仮定

# データは300点の系列
line_num <- 6
range <- 5
t <- 300 
z <- c(rep(NA, t))
trial <- rep(1, t)
Prob_beg <- .05
gamma <- rnorm(line_num, sd = .5)
# gamma <- rep(rnorm(line_num/2, sd = .5),2)

  for(k in 1:line_num){
    
    z <- c(rep(NA, t))
    trial <- rep(1, t)
    
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
    z[1] <- beta0 + beta * x[1] + gamma[k]
    
    # データの生成
    for(i in 2:range){
      beta0[i] <- rnorm(1,mean=beta0[i-1],sd=.03)
      z[i] <- beta * x[i] + rnorm(1, mean=beta0[i], sd=.03)
    }
    for(i in (range+1):length(x)) {
      beta0[i] <- rnorm(1,mean=beta0[i-1],sd=.03)
      z[i] <- beta * x[i] + beta_beg * beg10[i-range] + gamma[k] + rnorm(1, mean=beta0[i], sd=.03)
    }
    
    # zのlogisticが確率pとなる
    p <- 1/(1+exp(-z))    
    
    d <- data.frame(z=z,p=p,beg=beg,x=x, trial=trial)[-(1:range),]
    d$beg10 <- beg10
    
    # 確率pに応じたoutcomeが1/0で与えられる
    d$outcome <- rbinom(nrow(d), p = d$p, size=d$trial)
    
    if(k == 1) dat <- d
    if(k > 1) dat <- rbind(dat, d)
    
  }
  
d <- dat
  
d$id <- rep(1:line_num, each=nrow(d)/line_num)
# d$id <- rep(rep(1:(line_num/2), each=nrow(d)/line_num),2)
d$t <- rep(1:(t-range), line_num)

ggplot() +
  geom_point(data=d, aes(x=t, y=p, colour=as.factor(beg10))) +
  scale_color_manual(values = c("black","red")) +
  ylim(values=c(0,1)) + facet_grid(id~.) + theme(legend.position="none")

#-----stan sampling 実行-----#

outcome <- vec2mat(d$outcome, d$id)
x <- vec2mat(d$x, d$id)
beg10 <- vec2mat(d$beg10, d$id)
maxlen <- max(as.vector(table(d$id)))
trial <- vec2mat(d$trial, d$id)

d.list <- list(outcome=outcome,
               t=as.vector(table(d$id)), 
               x=x, 
               trial = trial, 　　　　　　　　　　　　　　　　　　　　　　　　　　　
               beg10 = beg10,
               id = d$id,
               n = length(unique(d$id)),
               maxlen <- maxlen)

fit <- stan(file = "22 binomial state space demo.stan",
            iter = 20000,
            warmup = 10000,
            chains = 3,
            thin = 5,
            data = d.list)

print(summary(fit)$summary[,"Rhat"], digit=3)
sum(summary(fit)$summary[,"Rhat"] > 1.1, na.rm=TRUE)

res <- extract(fit)

for(i in 1:length(unique(d$id))){
  if(i == 1) posteromean <- colMeans(res$p[,,i])
  if(i > 1) posteromean <- cbind(posteromean, colMeans(res$p[,,i]))
}

posteromean2 <- data.frame(posteromean = as.vector(posteromean),
                           id = as.factor(rep(1:ncol(posteromean), each=nrow(posteromean))),
                           t = rep(1:nrow(posteromean), ncol(posteromean)))

ggplot() +
  geom_point(data=d, aes(x=t, y=p, colour=as.factor(beg10))) +
  geom_line(data=posteromean2,aes(x=t, y=posteromean)) + 
  scale_color_manual(values = c("black","red")) +
  ylim(values=c(0,1)) + facet_grid(id~.) + theme(legend.position="none")



mean(res$beta)
beta

mean(res$beta_beg)
beta_beg

gamma
colMeans(res$gamma)

ggplot() +
  geom_point(aes(x=stack(as.data.frame(res$p))[,1]







