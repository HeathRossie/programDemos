setwd("~/Dropbox/R/demo/stan code")
#------元データの生成-----#
set.seed(1234)
N <- 10 # 個体数
rhp <- rnorm(N,0,5) # resource holding potential of each individual
n.data <- 500 # 対戦回数

# 勝敗データの生成
for(i in 1:n.data){
  pair <- sample(1:N, 2, replace = FALSE)
  rhp.diff <- rhp[pair[1]] - rhp[pair[2]]
  p.win <- 1/(1+exp(-rhp.diff))
  outcome <- rbinom(1, size = 1, prob = p.win)
  
  if(i == 1) d <- data.frame(ind1 = pair[1], ind2 = pair[2], outcome = outcome)
  if(i > 1) d <- rbind(d, c(pair, outcome))
}

#-----RHPを逆推定する-----#
library(rstan)
library(ggplot2)
library(dplyr)

d.list <- list(ind1 = d$ind1, ind2 = d$ind2, outcome = d$outcome, N = nrow(d), n = length(unique(c(d$ind1,d$ind2))))
fit <- stan(data = d.list,
            file = "19 BTmodel.stan",
            iter = 11000, warmup = 1000, chain =5,
            pars = c("gamma", "sigma"))

res <- extract(fit)
res$gamma %>% str

quantile2 <- function(x) return(quantile(x,c(.025,.975)))
ggplot() + 
  geom_point(aes(x=1:N, y=colMeans(res$gamma)), size = 5) +
  geom_errorbar(aes(x=1:N,ymin=apply(res$gamma,2,quantile2)[1,],ymax=apply(res$gamma,2,quantile2)[2,]), width=.1) +
  geom_point(aes(x=1:N+.1, y=rhp), size = 5, colour = "red") 

