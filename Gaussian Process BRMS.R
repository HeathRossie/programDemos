# install.packages('brms')
library(brms)
library(rstan)
library(ggplot2)
library(gridExtra)

x = runif(100, 0, 2*pi)
y = sin(x) + rnorm(100, sd = .1)

ggplot() + geom_point(aes(x=x, y=y))

rstan_options(auto_write=TRUE)
options(mc.cores = parallel::detectCores())

fit = brm(y ~ gp(x), chain = 4, iter=2000, warmup=1500, data=list(x=x,y=y))
summary(fit)

# posterior predictive distribution +/- 95% predictive intervals
xx = seq(0, 2*pi, length=100)
pred = predict(fit, data.frame(x=xx))
str(pred)

ggplot() + 
  geom_ribbon(aes(x=xx, ymin=pred[,3], ymax=pred[,4]), alpha=.3, fill="blue") + 
  geom_point(aes(x=x, y=y)) + 
  geom_line(aes(x=xx, y=pred[,1]), lwd=1.2, colour="blue")


# posteiror distribution +/- 95% credible intervals
me = marginal_effects(fit)
ggplot(me$x) + 
  geom_ribbon(aes(x=x, ymin=lower__, ymax=upper__), alpha=.5) +
  geom_line(aes(x=x,y=estimate__)) 


# compare conditions
x1 = runif(100, 0, 2*pi)
y1 = sin(x1) + rnorm(100, sd = .2)
x2 = runif(100, 0, 2*pi)
y2 = sin(x2) + cos(x2) + rnorm(100, sd = .2)

ggplot() + 
  geom_point(aes(x1, y1)) + 
  geom_point(aes(x2, y2), colour="red") 

x = c(x1, x2)
y = c(y1, y2)
f = factor(rep(c("A","B"), each=100))

rstan_options(auto_write=TRUE)
options(mc.cores = parallel::detectCores())

fit1 = brm(y ~ gp(x), chain=4, iter=2000, warmup=1500, data=list(x=x,y=y,f=f))
fit2 = brm(y ~ gp(x) + f, chain = 4, iter=2000, warmup=1500, data=list(x=x,y=y,f=f))
fit3 = brm(y ~ gp(x, by=f), chain = 4, iter=2000, warmup=1500, data=list(x=x,y=y,f=f))
waic(fit1, fit2, fit3)

summary(fit3)

xx = seq(0, 2*pi, length=100)
post_A = fitted(fit3, data.frame(x=xx, f="A"), summary=FALSE)
post_B = fitted(fit3, data.frame(x=xx, f="B"), summary=FALSE)
diffrenceAB = post_A-post_B
diffCI = data.frame(xx = xx,
                    est = apply(diffrenceAB,2, mean), 
                    lower = apply(diffrenceAB,2, function(x) quantile(x, 0.025)), 
                    upper = apply(diffrenceAB,2, function(x) quantile(x, 0.975)))
p_diff = ggplot(diffCI) + 
  geom_hline(yintercept = 0, lty=2) + 
  geom_ribbon(aes(x=xx, ymin=lower, ymax=upper), alpha = 0.4) +
  geom_line(aes(x=xx, y=est)) + xlab(NULL) + ylab(NULL) + 
  ggtitle("Posterior Distribution of Difference A-B (+/- 95%)")
p_diff

# posterior predictive
pred = predict(fit3, data.frame(x=rep(xx,2), f=f))
pred = as.data.frame(pred)
pred$f = f
pred$xx = rep(xx,2)

p_pred = ggplot() + 
  geom_ribbon(aes(x=pred$xx, ymin=pred[,3], ymax=pred[,4], fill=pred$f), alpha=.3) + 
  geom_point(aes(x=x, y=y, colour=f)) + 
  geom_line(aes(x=pred$xx, y=pred[,1], colour=pred$f), lwd=1.2) + 
  scale_fill_manual(values=c("red", "blue")) + 
  scale_colour_manual(values=c("red", "blue")) + 
  geom_text(aes(x=c(6.6,6.6), y=c(0.078, 1.07), label=c("A","B")), size=7) + 
  theme(legend.position = "none") + xlab(NULL) + ylab(NULL) + 
  ggtitle("Posterior Predictive (+/- 95%)")
p_pred

grid.arrange(p_pred, p_diff)
