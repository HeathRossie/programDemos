#-----みどりぼん10章-----#
# 場所差+個体差のある階層ベイズモデル

set.seed(1234)
library(rstan)
library(ggplot2)
library(dplyr)

#-----データの読み込み-----#
d <- read.csv("http://hosho.ees.hokudai.ac.jp/~kubo/stat/iwanamibook/fig/hbm/nested/d1.csv")
head(d)
str(d)

#-----データの図示-----#
# 個体ごとの差を見てみる
ggplot() + geom_text(data=d, aes(x=id, y=y, label=pot, colour=f)) +
  scale_colour_manual(values=c("black", "red")) +
  geom_hline(yintercept=mean(d[d$f=="C",]$y), colour="black", lty = 2) +
  geom_hline(yintercept=mean(d[d$f=="T",]$y), colour="red", lty = 2)

# pot差を見てみる
ggplot() + geom_boxplot(data=d, aes(x=pot, y=y, fill=f)) +
  scale_fill_manual(values=c("black", "red")) +
  geom_hline(yintercept=mean(d[d$f=="C",]$y), colour="black", lty = 2) +
  geom_hline(yintercept=mean(d[d$f=="T",]$y), colour="red", lty = 2)

#-----stanで推定してみる-----#
N_sample <- nrow(d)
N_pot <- length(levels(d$pot))
N_sigma <- 2

y <- d$y
f <- as.numeric(d$f == "T") # ダミー化のテク
pot <- as.numeric(d$pot) # 普通に数値化する

d.list=list(N_sample=N_sample, N_pot=N_pot, N_sigma=N_sigma,
  y=y, f=f, Pot=pot)

setwd("~/Dropbox/R/demo/stan code")
fit <- stan(file = "16 stancode.stan", iter = 11000,
  warmup = 1000, chain =3, data = d.list)
  
fit

#-----事後分布を見てみる-----#
result.stan <- extract(fit)
str(result.stan)
ggplot() + geom_density(aes(x=result.stan$beta2))
result.stan$beta2 %>% mean
# 効果はないと結論づけられそう
result.stan$beta2 %>% quantile(., c(.025,.975))

#-----他の慣れた方法だとどうなるか見てみる-----#

# t検定だと結果的には正しいけど・・・
t.test(d[d$f=="C",]$y, d[d$f=="T",]$y)

# ポアソン回帰だと誤った結論がでてくる
glm(y ~ f, data=d, family=poisson) %>% summary

# 個体差のみでGLMMでは誤った結論がでてくる
library(lme4)
glmer(y ~ f + (1|id), data=d, family=poisson) %>% summary

# 場所差のみのGLMMでも同じ今回の例だと正しい結果が得られる
glmer(y ~ f + (1|pot), data=d, family=poisson) %>% summary

# 今回のような単純な例だと最尤推定でも個体差・場所差両方組み込める
glmer(y ~ f + (1|pot) + (1|id), data=d, family=poisson) %>% summary

# パラメータ推定値も大体同じ値を返してくれる
glmer(y ~ f + (1|pot) + (1|id), data=d, family=poisson) %>% fixef
c(mean(result.stan$beta1), mean(result.stan$beta2))

