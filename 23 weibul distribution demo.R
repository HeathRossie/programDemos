library(rstan)
library(ggplot2)
library(dplyr)
setwd("/Users/MatsuiHiroshi/OneDrive/R/dataset")
d <- read.csv("contact sitting dataset.csv")
d <- data.frame(year = c(rep(1875, 7), rep(1876:2016, each=12)),
                month = c(6:12, rep(1:12, length(1876:2016))),
                ht = d$V2)

d$Dyad2 <- as.integer(d$Dyad)
d$Exp2 <- as.integer(factor(d$Exp,level=c("N","E"))) - 1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
d.list <- list(N=nrow(d),
               Sec=d$Sec,
               N_Dyad=length(unique(d$Dyad2)),
               Dyad=d$Dyad2,
               Exp=d$Exp2
               )
fit <- stan(file = "24 weibul distribution demo.stan",
            data = d.list,
            iter = 5250,
            warmup = 5000,
            chains = 4
)
 
res <- rstan::extract(fit)