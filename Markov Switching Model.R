# Markov Switching Model
# https://stackoverflow.com/questions/30281444/fitting-markov-switching-models-to-data-in-r
# This model is equivalent to Autoregressive hidden markov model
# when AR assumpution is added

library(MSwM)
?msmFit
y = c(rnorm(1000), rnorm(1000, 10))
y = matrix(c(y,y), ncol = 2)

msmFit(lm(y ~ 1), sw = c(TRUE,TRUE), k = 2)

library(rarhsmm)
em.hmm(y, mod, ntimes = NULL, tol = 1e-04, maxit = 100, arp = 0,
       cov.shrink = 0, auto.lambda = 0, auto.alpha = 0, print = TRUE)

set.seed(332213)
data(finance)
x <- data.matrix(finance)
#log return
y <- x[-1,-51]
for(i in 2:nrow(x)){
  y[i-1,] <- log(x[i,-51]) - log(x[i-1,-51])
}
#annualize the log return
y <- y * 252 

#first, fit a Gaussian HMM without autoregressive structure
m <- 2
#initialize the list of means
mu <- list(apply(y,2,mean), apply(y,2,mean))

#initialize the list of covariance matrices
sigma <- list(cov(y)*1.2,cov(y)*0.8)

#initialize the prior probability
delta <- c(0.5,0.5)

#initialize the transition probabilities
gamma <- matrix(c(0.9,0.1,0.2,0.8),2,2,byrow=TRUE)

mod1 <- list(m=m,mu=mu,sigma=sigma,delta=delta,gamma=gamma)
#will not run without a shrinkage on the covariance matrices because the 
#series is not long enough to reliably estimate the covariance structure
fit1 <- em.hmm(y=y,mod=mod1,cov.shrink=0.0001)
st1 <- viterbi.hmm(y=y,mod=fit1)
sp1 <- smooth.hmm(y=y,mod=fit1)

mod1 <- list(m=2,mu=c(0,0),sigma=matrix(sd(y), nrow=1),delta=c(0,0),gamma=gamma)
y =matrix(y, ncol = 1)
em.hmm(y=y,mod=mod1,cov.shrink=0.0001)
