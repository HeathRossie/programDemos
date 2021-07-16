# プロクラステス解析の demo
# サンプルデータは bill-extension 実験のハトデータ
# pre, S1, S2-4, S5-7, S8-10, post(after-effect), follow-upの7フェイズある
# loessで平滑化した座標を100等分した前処理が既に完了している

library(shapes)
library(ggplot2)
library(abind)
library(bigsplines)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
d_smooth <- read.csv("procrustes analysis demo.csv")[,-1]
str(d_smooth)

# k * m * nの配列を作る
# kはポイントの数
# mは次元数
# nはサンプルサイズ

# データフレームから3次元配列に変換
t <- 1
x <- cbind(d_smooth$x_upper_smooth, d_smooth$y_upper_smooth,
           d_smooth$x_lower_smooth, d_smooth$y_lower_smooth)[t:(t*100),]
x

for(i in 2:(nrow(d_smooth)/100)){
  t <- t+100
  y <- cbind(d_smooth$x_upper_smooth, d_smooth$y_upper_smooth,
             d_smooth$x_lower_smooth, d_smooth$y_lower_smooth)[t:(i*100),]
  
  if(i==2) ary <- array(cbind(x,y), dim=c(nrow(x), 4, i))
  if(i >2) ary <- abind(ary, y, along=3)
}


str(ary)
# Generalized procrustes analysisを実行
out1 <- procGPA(ary[,1:2,])
rot1 <- out1$rotated
out2 <- procGPA(ary[,3:4,])
rot2 <- out2$rotated

plot(1,1,xlim=c(min(rot1[,1,]), max(rot1[,1,])), ylim=c(min(rot1[,2,]), max(rot1[,2,])), type ="n")
for(i in 1:dim(rot1)[3]){
  points(rot1[,,i], cex=.3, pch=16)
}

d <- data.frame(serialnumber=rep(1:dim(rot1)[3],each=100),
                phase2=d_smooth$phase2,
                x_upper=stack(as.data.frame(rot1[,1,]))[,1], 
                y_upper=stack(as.data.frame(rot1[,2,]))[,1],
                x_lower=stack(as.data.frame(rot2[,1,]))[,1], 
                y_lower=stack(as.data.frame(rot2[,2,]))[,1])

ggplot() + geom_path(data=d,aes(x=x_upper,y=y_upper,group=serialnumber, colour=phase2)) + facet_wrap(~phase2)

# smooth spline ANOVAで曲線を条件間で比較する
fit <- bigssa(y_upper ~ x_upper * phase2, data=d,
              type=list(x_upper="per", phase2="nom"), 
              nknots=NULL,
              se.fit=TRUE)
fit$info[2] = cor(d$y, predict(fit))^2
fit$info

summary(fit)
fit$se.fit


ggplot() + geom_path(data=d[d$phase2=="A"|d$phase2=="P",],
                     aes(x=x_upper+.5,y=y_upper+.5,group=serialnumber, colour=phase2), alpha=.1) +
  
geom_path(data=d[d$phase2=="A"|d$phase2=="P",],
              aes(x=x_lower,y=y_lower,group=serialnumber, colour=phase2), alpha=.1) +
  scale_color_manual(values=c("red","blue"))
 

# original trajectory
ggplot() + geom_path(data=d_smooth[d_smooth$phase2=="A"|d_smooth$phase2=="P",],
                     aes(x=x_upper_smooth+.5,y=y_upper_smooth+.5,group=serialnumber, colour=phase2), alpha=.1) +
  
  geom_path(data=d_smooth[d_smooth$phase2=="A"|d_smooth$phase2=="P",],
            aes(x=x_lower_smooth,y=y_lower_smooth,group=serialnumber, colour=phase2), alpha=.1) +
  scale_color_manual(values=c("red","blue"))



