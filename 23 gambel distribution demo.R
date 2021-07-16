library(rstan)
library(ggplot2)
library(dplyr)
setwd("/Users/MatsuiHiroshi/OneDrive/R/dataset")
d <- read.csv("Highest temperature dataset.csv", header = FALSE)
d <- data.frame(year = c(rep(1875, 7), rep(1876:2016, each=12)),
           month = c(6:12, rep(1:12, length(1876:2016))),
           ht = d$V2)


ggplot(d) + 
  geom_histogram(aes(x=ht)) + 
  facet_wrap(~month)

d.list <- list(N=nrow(d), ht = d$ht, mon = d$month)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

stancode <- stan_model()
fit <- stan(file = "23 gambel distribution demo.stan",
            data = d.list,
            iter = 5250,
            warmup = 5000,
            chains = 4
            )
summary(fit)$summary[,"Rhat"][which(summary(fit)$summary[,"Rhat"] >= 1.1)]

res <- rstan::extract(fit)
str(res)
str(res$htpred)

htpred <- reshape2::melt(res$htpred)
ggplot(htpred) + 
  geom_histogram(aes(x=value)) + 
  facet_wrap(~Var2)

