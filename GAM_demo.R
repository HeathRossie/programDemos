#GAM
#これがメイン
library(mgcv)
#このへんもメジャー
library(gam)
library(gss) 
#ようわからんけどこういうのもあるらしい
library(assist)
library(gamlss) 
library(vgam) 

#gam()は基本的にはGLMと同じ書き方だけど平滑化関数
#s()かte()を指定する必要がある

data(trees)
head(trees) #桜の木のデータらしい
plot(scale(trees$Girth)[,1], trees$Volume)
points(scale(trees$Height)[,1], trees$Volume, pch = 16)
#どっちも明らかに関係はありそう

ct1 <- gam(Volume ~ s(Height) + s(Girth), data = trees,
           family = Gamma(link = log))
#これはつまりは
#log(Volume) = f1(Height) + f2(Girth)
#Volume ~ Gammaでfは平滑化関数 ということを示している
#平滑化の程度はGCVで決定
ct1
plot(ct1, residuals = TRUE)

ct1_h <- update(ct1, ~ s(Height))
ct1_g <- update(ct1, ~ s(Girth))
AIC(ct1, ct1_h, ct1_g)
anova(ct1, ct1_h, ct1_g, test = "F")
#AIC計算や検定もできるらしい

#s()はデフォルトではThin plate regressionというのを実行する
#TPRはデータが大きくなると計算が大変らしい
#bs = "cr" で罰則付きcubic spline (3次スプライン) にできる
ct2 <- gam(Volume ~ s(Height, bs = "cr") + s(Girth, bs = "cr"), 
           data = trees, family = Gamma(link = log))
ct2

#平滑化曲線の基底数 (k) はデフォルトでは０
#基底数の上限は自由度に依存
ct3 <- gam(Volume ~ s(Height, bs = "cr") + s(Girth, bs = "cr", k = 3), 
           data = trees, family = Gamma(link = log))
ct3

#gammaオプションは平滑化の程度らしくデフォルトは1だが
#オーバーフィッティング起こしやすいらしい
#Kim & Gu (2004) によれば1.4くらいがいいんじゃない？とのことらしい
ct4 <- gam(Volume ~ s(Height) + s(Girth), 
           data = trees, family = Gamma(link = log), gamma = 1.4)
ct4

#複数変数の平滑化
ct5 <- gam(Volume ~ s(Height, Girth, k = 25), 
           data = trees, family = Gamma(link = log), gamma = 1.4)
ct5
plot(ct5, too.far = 0.15)

#パラメトリックな項と混在させることもできる
ct6 <- gam(Volume ~ Height + s(Girth), 
           data = trees, family = Gamma(link = log), gamma = 1.4)
ct6
anova(ct6)

#別の例: Brain Imaging
library(gamair)
data(brain)
head(brain)
