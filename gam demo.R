 #-----スプライン補間demo-----#

set.seed(100)
x <- runif(1000, 0, 2*pi)
y <- sin(x) + rnorm(1000, 0, .4)

plot(x,y, pch = 16, cex = .4)

library(gam)
mod <- gam(y ~ s(x), family = 'gaussian')
pred <- data.frame(x=seq(min(x),max(x),length=1000))
pred$y <- predict(mod, pred)

lines(pred$x,pred$y,col = "red", lwd = 2)
lines(pred$x,sin(pred$x),col = "blue", lwd = 2)



x <- runif(1000, 0, 2*pi)
y <- sin(x) + rnorm(1000, 0, .4)
y[1:500] <- y[1:500] + .5
f <- factor(rep(c("A","B"), each = 500))

plot(x[1:500],y[1:500], pch = 16, cex = .4, col = "red", ylim = c(-1.5,2.5))
points(x[501:1000],y[501:1000], pch = 16, cex = .4, col = "blue", ylim = c(-1.5,2.5))

mod <- gam(y ~ s(x) + f, family = 'gaussian')
pred <- data.frame(x=rep(seq(min(x),max(x),length=500),2), f = f)
pred$y <- predict(mod, pred)

lines(pred$x[1:500],pred$y[1:500],col = "red", lwd = 2)
lines(pred$x[501:1000],pred$y[501:1000],col = "blue", lwd = 2)



mod0 <- gam(y ~ 1, family = 'gaussian')
mod1 <- mod <- gam(y ~ s(x), family = 'gaussian')
mod2 <- gam(y ~ s(x) + f, family = 'gaussian')
mod3 <- gam(y ~ s(x) * f, family = 'gaussian')
AIC(mod0,mod1,mod2, mod3)
anova(mod0,mod1,mod2,mod3)



