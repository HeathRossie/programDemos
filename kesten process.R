library(tidyverse)
library(gridExtra)


# creating time-series following Kesten process
y = 100 + rnorm(1)
N = 5000
alpha = 1.0 + rnorm(1, sd=0.01)
beta = rnorm(1)

for(i in 1:N){
  y_new = alpha[i] * y[i] + beta[i]
  alpha = c(alpha, 1.0 + rnorm(1, sd=0.01))
  beta = c(beta, rnorm(1, sd=0.01))
  y = c(y, y_new)
}

d = data.frame(time = 1:(N+1), y = y, alpha = alpha, beta = beta)[-1,]


grid.arrange(# time series
             ggplot(d) + geom_line(aes(x=time, y=y)),
             # density
             ggplot(d) + geom_density(aes(x=y)),
             # normal q-q plot
             ggplot(d, aes(sample = y)) +
               stat_qq() +
               stat_qq_line()
             )


