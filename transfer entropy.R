library(ggplot2)
library(TransferEntropy)
library(dplyr)

n = 1500
x = rnorm(1)
y = rnorm(1)
tau = 25
w = .7

for(i in 2:n){
  if(i <= tau){
    x = c(x, rnorm(1, mean = x[i-1]))
    y = c(y, rnorm(1, mean = w * x[i-1] + (1-w) * y[i-1]))
  }else{
    x = c(x, rnorm(1, mean = x[i-1]))
    y = c(y, rnorm(1, mean = w * x[i-tau] + (1-w) * y[i-1]))
  }
  
}

x = x[501:1500]
y = y[501:1500]


# ggplot() +
#   geom_line(aes(x=1:length(x), y=x), colour = "black") +
#   geom_line(aes(x=1:length(y), y=y), colour = "red")
# 
# ## Compute the TE from Y to X
# computeTE(x, y, embedding=3, k=1, "MI_diff")
# ## Compute the TE from X to Y
# computeTE(y, x, embedding=3, k=1, "MI_diff")


## The Transfer Delay is taken into account
delay = 50
Y_to_X = computeTE(x, y, embedding=3, k=1, "MI_diff")
X_to_Y = computeTE(y, x, embedding=3, k=1, "MI_diff")

for(i in 1:delay){
  Y_to_X = c(Y_to_X, 
             computeTE(x[-( (length(x)-i+1):(length(x)) )], y[-(1:i)], embedding=3, k=1, "MI_diff")
  )
  
  X_to_Y = c(X_to_Y, 
             computeTE(y[-( (length(x)-i+1):(length(x)) )], x[-(1:i)], embedding=3, k=1, "MI_diff")
  )
}

Y_to_X = unlist(Y_to_X)
X_to_Y = unlist(X_to_Y)


ggplot() + 
  geom_line(aes(x=1:length(Y_to_X), y=Y_to_X), colour="black") + 
  geom_line(aes(x=1:length(X_to_Y), y=X_to_Y), colour="red") 
