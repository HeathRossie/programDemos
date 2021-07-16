# Bayesian Analysis of Factorial Designsという論文 Rounder et al.
# http://pcl.missouri.edu/exp/aovExample.txt and can be read into R using the read.table function:

dat=read.table(url('http://pcl.missouri.edu/exp/aovExample.txt'), head=TRUE)

dat$s=factor(dat$s)
dat$a=factor(dat$a)
dat$d=factor(dat$d)
dat$p=factor(dat$p)

head(dat)
library(ggplot2)
ggplot() +
  geom_boxplot(data=dat,aes(x=d,y=rt)) + facet_wrap(~p)

aovResult=aov(rt ~ a*d*p+Error(s/(d*p)),data=dat) 
summary(aovResult)

library(BayesFactor)
bf = anovaBF(rt ~ a+d+p+s, data = dat, whichModels="withmain",
             whichRandom = "s", iterations = 1000)
bf=sort(bf, decreasing = TRUE)
plot(head(bf))
bf

?anovaBF

# 回帰モデル用のもあるらしい
data(attitude)
plot(attitude)
head(attitude)
summary(fm1 <- lm(rating ~ ., data = attitude))

output = regressionBF(rating ~ complaints + learning + raises + critical + advance + privileges, data = attitude, progress=FALSE)
plot(output)
head(output)

attitude$f <- factor(sample(c("A","B","C"), nrow(attitude), replace=TRUE))
attitude[attitude$f == "A", ]$rating <- attitude[attitude$f == "A", ]$rating + sd(attitude[attitude$f == "A", ]$rating)

output = regressionBF(rating ~ complaints, data = attitude, progress=FALSE)
output = generalTestBF(rating ~ complaints + f, data = attitude, progress=FALSE)
plot(output)
str(output)
output@numerator$`complaints + f`

c <- posterior(output, index = "BFlinearModel", iterations = 10000)
head(c)
colMeans(c)

summary(output)

str(ToothGrowth)

full <- lmBF(len ~ supp + dose + supp:dose, data=ToothGrowth)
chainsFull <- posterior(full, iterations = 10000)
summary(chainsFull[,1:7])

coef(lm(len ~ supp + dose + supp:dose, data=ToothGrowth))

str(chainsFull)
chainsFull@data
head(chainsFull)
dim(chainsFull)

plot(full)

