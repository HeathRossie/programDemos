library(lme4)
library(multcomp)
library(car)
iter <- 1000
p <- NULL
p2 <- NULL
for(i in 1:iter){
  n <- 5
  trial <- 40
  f <- factor(rep(LETTERS[1:4], 50))
  id <- factor(rep(1:n, each=trial))
  r <- rnorm(n)
  r <- rep(r, each=40, sd = 1)
  data <- rnorm(n*trial, mean=r, sd = .2)
 
  
  mod <- lmer(data ~ f-1 + (1|id))
  mod2 <- lmer(data ~ f-1 + (f|id))
  p <- c(p, sum(summary(glht(mod, linfct=mcp(f= "Tukey")))[[10]]$pvalues < .05))
  p2 <- c(p2, sum(summary(glht(mod2, linfct=mcp(f= "Tukey")))[[10]]$pvalues < .05))
  print(paste(i/iter * 100, "% completed"))
}

plot(data, col=factor(id))
sum(p > 0)/iter
sum(p2 > 0)/iter

