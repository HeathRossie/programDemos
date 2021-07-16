# Bayesian Ordination and Regression Analysis of Multivariate Abundance Data in R
# Methods in Ecology and Evolutionで紹介されているパッケージ

library(rjags)
library(mvabund)
library(boral)
data("spider")
str(spider)
y <- spider$abund
fit.lvmp <- boral(y=y, family = "poisson", num.lv = 2, row.eff = "fixed")
summary(fit.lvmp)
fit.lvmp$hpdintervals
# overdispersionが確認できる
plot(fit.lvmp)

# そういうときは分布を負の二項分布にするとある程度は対応できる
fit.lvmp <- boral(y=y, family = "negative.binomial", num.lv = 2, row.eff = "fixed")
lvsplot(fit.lvmp)

# 共変量を考慮することも可能
X <- scale(spider$x)
fit.Xnb <- boral(y = y, X = X, family = "negative.binomial", num.lv = 2, save.model = TRUE)
summary(fit.Xnb)
plot(fit.Xnb)
