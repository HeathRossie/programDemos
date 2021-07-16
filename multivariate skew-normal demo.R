library(ggplot2)
library(mixsmsn)
library(dplyr)

# set parameters
mu1 <- c(0, 0)
Sigma1 <- matrix(c(3, -4, 2, 5), 2, 2)
shape1 <-c(4, 4)
nu1 <- 4

mu2 <- c(5, 5)
Sigma2 <- matrix(c(2, 1, 1, 2), 2, 2)
shape2 <-c(2, 2)
nu2 <- 4
pii <- c(0.6, 0.4)

arg1 <- list(mu = mu1, Sigma = Sigma1, shape = shape1, nu = nu1)
arg2 <- list(mu = mu2, Sigma = Sigma2, shape = shape2, nu = nu2)

# multivariate
y <- rmmix(n = 1000, p = pii, family = "Skew.normal",
              arg = list(arg1, arg2))

# univariate
y <- rmmix(n = 1000, p = 1, family = "Skew.normal",
           arg = list(arg1))

ggplot() +
  geom_point(aes(x=y[,1],y=y[,2]), size = .5) +
  geom_density2d(aes(x=y[,1],y=y[,2]), lwd = 2)

# fitting
mu <- list(mean(y[,1]),mean(y[,1]))
Sigma <- list(Sigma1)
shape <- list(shape1)
pii <- 1

Norm.analysis <- smsn.mmix(y, nu=1, mu=mu, Sigma=Sigma, shape=shape, pii = pii,
                           criteria = TRUE, g=2, get.init = FALSE, group = TRUE,
                           family = "Normal")

res <- smsn.mmix(y, # nu = 1,  mu = mu, Sigma = Sigma, shape = shape, pii = 1,
          g = 1,
          family = "Skew.normal", group = TRUE)
str(res)
res$mu
mu1
res$Sigma
Sigma1
mix.contour(y,res, ncontour = 30)
points(res$mu[[1]][1],res$mu[[1]][2], col = "blue", cex=2, pch=16)
ggplot() +
  geom_point(aes(x=y[,1],y=y[,2]), size = .5) +
  geom_density2d(aes(x=y[,1],y=y[,2]), lwd = 2) + 
  geom_point(aes(x=res$mu[[1]][1], y=res$mu[[1]][2]), colour="red", size=3) 
  
