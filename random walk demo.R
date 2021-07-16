library(rstan)
library(ggplot2)
#
t <- 12
n <- 1000

#y <- rep(0,n)
#y <- cbind(y,rnorm(n, mean = y))  
#for(i in 3:t){
#  y <- cbind(y, rnorm(n, mean = y[,i-1]))
#}

#y <- t(y)

library(ggplot2)

#rownames(y) <- NULL
#colnames(y) <- NULL
#y <- unlist(y)
#y <- stack(as.data.frame(y))[,1]
#dat <- data.frame(t = rep(1:t, n), y = y, seq = rep(1:n, each=t))
setwd("~/Dropbox/R/demo/demo/statistics")
dat <- read.csv("random walk row.csv")[,-1]

ggplot() + 
  geom_line(data=dat, aes(x=t, y=y, group=seq), colour="gray10", lwd=.2, alpha=.5)

p.value <- NULL
for(i in 1:n){
  temp <- dat[dat$seq == i,]
  result <- summary(lm(y ~ t, temp))
  f.stat <- result$fstatistic
  p.value <- c(p.value, 1-pf(f.stat["value"],f.stat["numdf"],f.stat["dendf"]))
}

sign <- rep(0, n)
sign[p.value < .05] <- 1
sum(sign)/length(sign)
sign <- rep(sign, each = t)
dat$sign <- sign

ggplot() + 
  geom_line(data=dat[dat$sign==1,], aes(x=t, y=y, group=seq), colour="red", lwd=.2, alpha=.5) +
  geom_line(data=dat[dat$sign==0,], aes(x=t, y=y, group=seq), colour="gray10", lwd=.2, alpha=.5) 

d <- read.csv("random walk.csv")[,-1]

#iter <- 1000
#mod <- stan_model("radam walk demo.stan")
#for(i in 1:iter){
#d.list <- list(n = nrow(dat[dat$seq==i,]), t = 1:length(dat[dat$seq==i,]$t),  y = dat[dat$seq==i,]$y)

#fit <- sampling(mod,
#                iter = 11000, warmup = 1000,
#                data = d.list, thin = 2)
#
#res <- extract(fit)


#quantile(res$beta, prob=c(.025,.975))

#if(i==1) beta <- res$beta
#if(i > 1) beta <- cbind(beta, res$beta)

#print(paste(i/iter  * 100, "% completed"))
#}


#quantile2 <- function(x) return(quantile(x, prob=c(.025, .975)))
#posteromean <- apply(beta, 2, mean)
#posteroCI <- t(apply(beta, 2, quantile2))

#d <- data.frame(mean = posteromean, lower = posteroCI[,1], upper = posteroCI[,2])
#d

#sum(d[,2] < 0 & d[,3] > 0)
#d$TypeI <- 1
#d[d[,2] < 0 & d[,3] > 0,]$TypeI <- 0
d$TypeI <- as.factor(d$TypeI)

ggplot() +
  geom_point(aes(x=d[,1][order(d[,1])], y=1:nrow(d), colour = d$TypeI[order(d[,1])]), size=.1) +
  geom_errorbarh(aes(x=d[,1][order(d[,1])], xmin = d[,2][order(d[,1])], xmax = d[,3][order(d[,1])], y=1:nrow(d), colour = d$TypeI[order(d[,1])]), height = 0, lwd = .3) +
  scale_colour_manual(values=c("black", "red")) + theme(legend.position = "none")
  
sum(p.value[1:100] < .05) /100
sum(as.integer(d$TypeI)-1)/1000


slope <- NULL
lower <- NULL
upper <- NULL
for(i in 1:1000){
  slope <- c(slope, coef(lm(y ~ t, data = dat[dat$seq==i,]))[2])
  lower <- c(lower, confint(lm(y ~ t, data = dat[dat$seq==i,]))[2,1])
  upper <- c(upper, confint(lm(y ~ t, data = dat[dat$seq==i,]))[2,2])
}

d_lm <- data.frame(mean = slope, lower = lower, upper = upper)
d_lm$TypeI <- 1
d_lm[d_lm[,2] < 0 & d_lm[,3] > 0,]$TypeI <- 0
d_lm$TypeI <- as.factor(d_lm$TypeI)

ggplot() +
  geom_point(aes(x=d_lm[,1][order(d_lm[,1])], y=1:nrow(d_lm), colour = d_lm$TypeI[order(d_lm[,1])]), size=.1) +
  geom_errorbarh(aes(x=d_lm[,1][order(d_lm[,1])], xmin = d_lm[,2][order(d_lm[,1])], xmax = d_lm[,3][order(d_lm[,1])], y=1:nrow(d_lm), colour = d_lm$TypeI[order(d_lm[,1])]), height = 0, lwd = .3) +
  scale_colour_manual(values=c("black", "red")) + theme(legend.position = "none")

plot(d$mean, d_lm$mean)
points(d[d$TypeI==1,]$mean, d_lm[d$TypeI==1,]$mean, col = "blue", pch=16)
points(d[d_lm$TypeI==1,]$mean, d_lm[d_lm$TypeI==1,]$mean, col = "green", pch=16)
abline(a=0,b=1, col = "red")

ggplot() +
  geom_point(aes(x=d[,1][order(d[,1])], y=1:nrow(d), colour = d$TypeI[order(d[,1])]), size=.1) +
  geom_errorbarh(aes(x=d[,1][order(d[,1])], xmin = d[,2][order(d[,1])], xmax = d[,3][order(d[,1])], y=1:nrow(d), colour = d$TypeI[order(d[,1])]), height = 0, lwd = .3) +
  scale_colour_manual(values=c("black", "red")) + theme(legend.position = "none")



write.csv(dat, "random walk row.csv")
write.csv(d, "random walk.csv")
